from collections import defaultdict, Counter
from itertools import chain
import re


class ReductsGenerator:
    def __init__(self, filename="db.pl") -> None:
        self.db = filename
        self.traits = []

    def get_disc_matrix(self) -> list:
        """
        Generates discernibility matrix. Each row is a list of lists of traits that differ from the row being compared.
        """
        disease_levels = defaultdict(list)

        with open(self.db, "r") as f:
            pattern = r"\w*\(\w+\s*,\s*(?:\d+(?:\.\d+)?|\w+)\)"
            content = f.read()
            results = re.findall(pattern, content)

        for result in results:
            trait, disease, level = re.match(r"(\w*)\((\w+)\s*,\s*(\d+(?:\.\d+)?|\w+)\)", result).groups()
            disease_levels[disease].append(level)
            self.traits.append(trait) if trait not in self.traits else None

        full_matrix = [[*levels, disease] for disease, levels in disease_levels.items()]

        reduced_matrix = self._reduce_matrix(full_matrix)
        diff_matrix = self._generate_diff_matrix(reduced_matrix)
        return diff_matrix

    def _reduce_matrix(self, matrix) -> list:
        """
        Reduces matrix by removing duplicate rows.
        """
        reduced_dict = {}

        for row in matrix:
            order = tuple(row[:-1])
            if order not in reduced_dict:
                reduced_dict[order] = []
            reduced_dict[order].append(row[-1])

        reduced_matrix = [list(k) + [v] for k, v in reduced_dict.items()]

        return reduced_matrix

    def _generate_diff_matrix(self, matrix) -> list:
        """
        Generates diff matrix. Each row is a list of lists of traits that differ from the row being compared.
        """
        diff_matrix = []

        for row in matrix:
            diff_matrix.append(
                [[self.traits[j] for j in range(len(row[:-1])) if row[j] != row_compare[j]] for row_compare in matrix]
            )

        return diff_matrix

    def get_reducts(self, diff_matrix) -> list:
        """
        Returns list of reducts. Implements Johnson's heurestic.
        """
        reducts = []

        while len(diff_matrix) > 0:
            all_traits = list(chain.from_iterable(chain.from_iterable(diff_matrix)))
            traits_count = Counter(all_traits)
            max_frequency = traits_count.most_common(1)[0][0]
            reducts.append(max_frequency)

            diff_matrix = [[col for col in row if max_frequency not in col and col] for row in diff_matrix]
            diff_matrix = [row for row in diff_matrix if row]

        return reducts


if __name__ == "__main__":
    generator = ReductsGenerator()
    matrix = generator.get_disc_matrix()
    reducts = generator.get_reducts(matrix)

    with open("redukty.pl", "w") as f:
        f.write(":- dynamic reduct/1.\n\n")
        for reduct in reducts:
            f.write(f"reduct({reduct})." + "\n")
