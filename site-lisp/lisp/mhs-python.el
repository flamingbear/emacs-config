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
;;; basedpyright and ruff are tools, not project dependencies.  Install them
;;; once, outside any project env, so there is a copy every project can reach:
;;;
;;;   uv tool install basedpyright
;;;   uv tool install ruff
;;;
;;; Lookup order is: the project virtualenv (pyvenv puts it at the head of
;;; `exec-path' on activation), then the standalone install.

(defcustom mhs-python-tool-bin (expand-file-name "~/.local/bin")
  "Directory holding standalone Python tools (`uv tool install', pipx)."
  :type 'directory
  :group 'mhs)

(defcustom mhs-conda-envs-directories
  (list "/opt/homebrew/Caskroom/miniforge/base/envs/"
        (expand-file-name "~/.conda/envs/"))
  "Directories conda/mamba keep named environments in.
The first is where `brew install --cask miniforge' puts them; check
`conda info --envs' if yours differ."
  :type '(repeat directory)
  :group 'mhs)

(defun mhs-python-tool-path (name)
  "Absolute path to Python tool NAME, or nil if it cannot be found.

Checks `exec-path' first, so an active virtualenv\='s own copy wins and a
project can still pin its own version.  `mhs-python-tool-bin\=' is checked
directly rather than through `exec-path\=', so it works whether or not it
is on PATH."
  (let ((standalone (expand-file-name name mhs-python-tool-bin)))
    (or (executable-find name)
        (and (file-executable-p standalone) standalone))))


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

(defun mhs-python--conda-env-dir (name)
  "Directory of the conda env called NAME, or nil if it has no python."
  (seq-some (lambda (root)
              (let ((env (expand-file-name name root)))
                (and (file-executable-p (expand-file-name "bin/python" env))
                     env)))
            mhs-conda-envs-directories))

(defun mhs-python--environment-yml-name ()
  "The `name:' from the nearest environment.yml (or .yaml), or nil."
  (when-let* ((dir (locate-dominating-file
                    default-directory
                    (lambda (d)
                      (or (file-exists-p (expand-file-name "environment.yml" d))
                          (file-exists-p (expand-file-name "environment.yaml" d))))))
              (file (seq-find #'file-exists-p
                              (list (expand-file-name "environment.yml" dir)
                                    (expand-file-name "environment.yaml" dir)))))
    (with-temp-buffer
      (insert-file-contents file)
      (when (re-search-forward "^name:[ \t]*['\"]?\\([^'\"# \t\n]+\\)" nil t)
        (match-string 1)))))

(defun mhs-python--python-version-env-name ()
  "Env name from the nearest .python-version, or nil for a bare version.

Left over from pyenv, which kept env names here; old files may hold a
path like \"miniforge3-26.1.1-3/envs/papermill-hoss\", so only the last
component is used.  `uv python pin' writes a bare version (\"3.12\") into
this same file, and that is not an env name, so it is skipped."
  (when-let* ((dir (locate-dominating-file default-directory ".python-version"))
              (name (string-trim
                     (with-temp-buffer
                       (insert-file-contents
                        (expand-file-name ".python-version" dir))
                       (buffer-string)))))
    (unless (or (string-empty-p name)
                (string-match-p "\\`[0-9]+\\(\\.[0-9]+\\)*\\'" name))
      (file-name-nondirectory (directory-file-name name)))))

(defun mhs-python--conda-env ()
  "Conda env for this project, named by environment.yml or .python-version."
  (seq-some (lambda (name) (and name (mhs-python--conda-env-dir name)))
            (list (mhs-python--environment-yml-name)
                  (mhs-python--python-version-env-name))))

(defun mhs-python-activate-venv ()
  "Activate the virtualenv belonging to this buffer's project.

Prefers a `.venv' directory (uv, `python -m venv'), then the miniforge
conda env named by the nearest environment.yml or .python-version.
Silent no-op when neither is found, so projects driven some other way
are left alone."
  (interactive)
  (when-let ((venv (or (mhs-python--dot-venv) (mhs-python--conda-env))))
    (unless (and (bound-and-true-p pyvenv-virtual-env)
                 (file-equal-p venv pyvenv-virtual-env))
      (pyvenv-activate venv))))

;; Depth -90 so this runs before the lsp-deferred hook below and any LSP
;; process inherits the right `exec-path'.
(add-hook 'python-base-mode-hook #'mhs-python-activate-venv -90)


;;; ---------------------------------------------------------------------------
;;; LSP clients

(defun mhs-python-refresh-lsp-tools ()
  "Point the ruff and basedpyright clients at absolute executable paths.
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
   "venv: %s\npython: %s\nruff: %s\nbasedpyright-langserver: %s"
   (or (bound-and-true-p pyvenv-virtual-env) "none active")
   (or (executable-find "python") "not found")
   (or (mhs-python-tool-path "ruff")
       "not found -- `uv tool install ruff'")
   (or (mhs-python-tool-path "basedpyright-langserver")
       "not found -- `uv tool install basedpyright'")))


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


;;; ---------------------------------------------------------------------------
;;; Modeline python version
;;;
;;; doom-modeline's python env segment otherwise asks whatever `python' it
;;; finds first, which need not be the one pyvenv activated.  Ask the
;;; interpreter the buffer will actually use instead.

(defun mhs-python--modeline-env-command ()
  "Version command for doom-modeline's python segment.
Resolves `python-shell-interpreter' through `exec-path', so an active
pyvenv virtualenv (uv .venv or conda env) reports its own version."
  (list (or (executable-find python-shell-interpreter)
            python-shell-interpreter)
        "--version"))

(with-eval-after-load 'doom-modeline-env
  (setq doom-modeline-env-python-command #'mhs-python--modeline-env-command))



(use-package python-pytest
  :ensure t
  :after python
  :bind (:map python-base-mode-map
              ("C-c y" . python-pytest-dispatch))
  :config
  (transient-append-suffix 'python-pytest-dispatch "-q"
    '("-R" "show output of passing tests" "-rfEP")))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))


(provide 'mhs-python)
;;; mhs-python.el ends here
