;;; mhs-python.el --- Python environment, LSP tooling, virtualenv activation  -*- lexical-binding: t; -*-

;; Below: Stolen from KWB totally^H^H^H^H^H mostly (well, originally anyway).


;; Switch to tree-sitter mode [claude says so] 2026-01-29
(when (treesit-language-available-p 'python)
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(with-eval-after-load 'smartparens
  (add-hook 'python-base-mode-hook #'smartparens-mode))

;; We're gonna need us a Python mode
(use-package python)


;;; ---------------------------------------------------------------------------
;;; Language server executables
;;;
;;; basedpyright and ruff are tools, not project dependencies.  `pip install'ing
;;; them into a pyenv version creates a shim in ~/.pyenv/shims that shadows every
;;; other copy on PATH, and that shim only resolves for the pyenv version it was
;;; installed under.  In any other project it dies with:
;;;
;;;   pyenv: basedpyright-langserver: command not found
;;;   The `basedpyright-langserver' command exists in these Python versions: ...
;;;
;;; Install them once, outside pyenv, so there is a copy that every project can
;;; reach:
;;;
;;;   uv tool install basedpyright
;;;   uv tool install ruff
;;;
;;; Lookup order is: the project virtualenv (pyvenv puts it at the head of
;;; `exec-path' on activation), then the standalone install.  A shim is only
;;; reached by falling through to lsp-mode's own `executable-find' -- which is
;;; exactly the behaviour you had before, so nothing gets worse if neither of
;;; the first two is present.

(defcustom mhs-python-tool-bin (expand-file-name "~/.local/bin")
  "Directory holding standalone Python tools (`uv tool install', pipx)."
  :type 'directory
  :group 'mhs)

(defcustom mhs-pyenv-versions-directory (expand-file-name "~/.pyenv/versions/")
  "Directory pyenv keeps its versions and virtualenvs in."
  :type 'directory
  :group 'mhs)

(defun mhs-python--shim-p (path)
  "Return non-nil when PATH is a pyenv shim."
  (and path (string-match-p "/\\.pyenv/shims/" path)))

(defun mhs-python-tool-path (name)
  "Absolute path to Python tool NAME, or nil if the only copy is a pyenv shim.

Checks `exec-path' first, so an active virtualenv\='s own copy wins and a
project can still pin its own version.  A shim found there is rejected in
favour of `mhs-python-tool-bin\=', because a shim resolves against whichever
pyenv version is selected and fails in every project that does not have
NAME installed.

`mhs-python-tool-bin\=' is checked directly rather than through
`exec-path\=', so it works whether or not it is on PATH."
  (let ((found (executable-find name))
        (standalone (expand-file-name name mhs-python-tool-bin)))
    (cond ((and found (not (mhs-python--shim-p found))) found)
          ((file-executable-p standalone) standalone))))


;;; ---------------------------------------------------------------------------
;;; Per-project virtualenv activation
;;;
;;; This replaces the per-project .dir-locals.el.  Those only listed
;;; `python-mode', and `python-ts-mode' is a sibling of `python-mode' (both
;;; derive from `python-base-mode'), not a child -- so once the tree-sitter
;;; remap above went in, dir-locals stopped firing and nothing activated the
;;; venv any more.

(defun mhs-python--dot-venv ()
  "Nearest PEP 405 / uv style .venv directory above `default-directory'."
  (when-let* ((dir (locate-dominating-file default-directory ".venv/"))
              (venv (expand-file-name ".venv" dir)))
    (and (file-executable-p (expand-file-name "bin/python" venv)) venv)))

(defun mhs-python--pyenv-env ()
  "Pyenv virtualenv named by the nearest .python-version file, or nil."
  (when-let* ((dir (locate-dominating-file default-directory ".python-version"))
              (name (string-trim
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name ".python-version" dir))
                       (buffer-string)))))
    ;; `uv python pin' writes a bare version ("3.12") into this same file.
    ;; ~/.pyenv/versions/3.12 can exist and is a bare interpreter carrying none
    ;; of the project's dependencies, so activating it fails silently -- every
    ;; import breaks with no error pointing at the environment.  Bare versions
    ;; are not env names; skip them.
    (unless (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)*\\'" name)
      (let ((env (expand-file-name name mhs-pyenv-versions-directory)))
        (and (file-executable-p (expand-file-name "bin/python" env)) env)))))

(defun mhs-python-activate-venv ()
  "Activate the virtualenv belonging to this buffer's project.

Prefers a `.venv' directory (uv, `python -m venv'), then the pyenv
virtualenv or conda env named by the nearest `.python-version'.  Silent
no-op when neither is found, so projects driven some other way are left
alone."
  (interactive)
  (when-let ((venv (or (mhs-python--dot-venv) (mhs-python--pyenv-env))))
    (unless (and (bound-and-true-p pyvenv-virtual-env)
                 (file-equal-p venv pyvenv-virtual-env))
      (pyvenv-activate venv))))

;; Depth -90 so this runs before the lsp-deferred hook below and any LSP
;; process inherits the right `exec-path'.
(add-hook 'python-base-mode-hook #'mhs-python-activate-venv -90)


;;; ---------------------------------------------------------------------------
;;; LSP clients

(defun mhs-python-refresh-lsp-tools ()
  "Point the ruff and basedpyright clients at real, non-shim executables.
Run again after installing either tool; no Emacs restart needed."
  (interactive)
  (let ((ruff (mhs-python-tool-path "ruff"))
        (pyright (mhs-python-tool-path "basedpyright-langserver")))
    ;; Safe to set before lsp-ruff.el loads: `defcustom' does not clobber a
    ;; value the variable already has.
    (when ruff
      (setq lsp-ruff-server-command (list ruff "server")))
    ;; lsp-pyright builds the dependency name by appending "-langserver" to
    ;; `lsp-pyright-langserver-command', then resolves it with `executable-find'.
    ;; Re-registering with an absolute path takes PATH out of the picture.
    ;; `lsp-pyright-langserver-command' itself must stay the bare name -- it is
    ;; also the prefix for every setting key ("basedpyright.typeCheckingMode").
    ;; This one does need lsp-pyright loaded, since it edits that package's
    ;; dependency table; the hook below re-runs us once it is.
    (when (and pyright (featurep 'lsp-pyright))
      (lsp-dependency 'pyright (list :system pyright)))
    (list :ruff ruff :basedpyright pyright)))

(defun mhs-python-lsp-doctor ()
  "Report how the Python LSP tooling resolves in the current buffer."
  (interactive)
  (message
   (concat "venv: %s\npython: %s\nruff: %s\nbasedpyright-langserver: %s%s")
   (or (bound-and-true-p pyvenv-virtual-env) "none active")
   (or (executable-find "python") "not found")
   (or (mhs-python-tool-path "ruff")
       "no standalone copy -- lsp will fall back to PATH")
   (or (mhs-python-tool-path "basedpyright-langserver")
       "no standalone copy -- lsp will fall back to PATH")
   (if (mhs-python--shim-p (executable-find "basedpyright-langserver"))
       "\nnote: PATH resolves this to a pyenv shim; `uv tool install basedpyright'"
     "")))


(use-package lsp-pyright
  :ensure t
  :defer t
  :custom (lsp-pyright-langserver-command "basedpyright")
  :hook (python-base-mode . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))

;; These bodies close over nothing, so they are safe regardless of when they
;; run.  lsp-pyright is required lazily from the mode hook above, which is why
;; it needs its own entry.
(with-eval-after-load 'lsp-mode
  (mhs-python-refresh-lsp-tools))

(with-eval-after-load 'lsp-pyright
  (mhs-python-refresh-lsp-tools))


(use-package python-pytest
  :ensure t
  :after python
  :bind (:map python-base-mode-map
              ("C-c y" . python-pytest-dispatch)))


(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))


(provide 'mhs-python)
;;; mhs-python.el ends here
