import argparse as ap


def parse_arguments():
    parser = ap.ArgumentParser(description="Put description here.")
    parser.add_argument('first_argument', help='description of first_argument.')
    parser.add_argument('second_argument', help='describe second_argument')
    return parser.parse_args()


if __name__ == '__main__':
    args = parse_arguments()
    first_argument = args.first_argument
    second_argument = args.second_argument
