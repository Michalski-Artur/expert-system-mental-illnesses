from copy import deepcopy
import re

DATABASE_FILE = 'db.pl'


def traits():
    with open (DATABASE_FILE, 'r') as f:
        content = f.read()
        pattern = r"\w+\("
        results = re.findall(pattern, content)
        final = []
        
        for result in results:
            result = result[:len(result)-1] 
            if result not in final:
                final.append(result)

        return final


def reduce_matrix(matrix):
    rows_to_del = []
    result = []
    
    for idx, row in enumerate(matrix):
        if idx in rows_to_del:
            continue
        
        identical = []
        order = row[:len(row)-1]
        
        for idx_compare, row_compare in enumerate(matrix[idx+1:]):
            order_compare = row_compare[:len(row_compare)-1]
             
            if order == order_compare:
                rows_to_del.append(idx_compare+idx+1)

                if row[-1] != row_compare[-1]:
                    identical.append(row_compare[-1])

        if type(row[-1]) == list:
            row[-1].extend(identical)
        else:
            row[-1] = [row[-1]] + identical 

        result.append(row)

    return result


def make_matrix():
    full_matrix = []
    diseases_indeces = {}

    with open (DATABASE_FILE, 'r') as f:
        content = f.read()
        params = traits()

        for idx, param in enumerate(params):
            pattern = re.escape(param) + r"\(\w+\,\w+"
            results = re.findall(pattern, content)
            levels = [result.split('(')[1].split(',')[1] for result in results]
            diseases = [result.split('(')[1].split(',')[0] for result in results]

            if idx == 0:
                for idx_level, level in enumerate(levels):
                    full_matrix.append([level])
                
                for idx_disease, disease in enumerate(diseases):
                    full_matrix[idx_disease].append(disease)
                    diseases_indeces[disease] = idx_disease

            else:
                for idx_disease, disease in enumerate(diseases):
                    idx_level = diseases_indeces[disease]
                    full_matrix[idx_level].insert(len(full_matrix[idx_level])-1, levels[idx_level])

    
    print("####### MACIERZ ROZRÓŻNIALNOŚCI NIEZREDUKOWANA #######")
    for row in full_matrix:
        print(row)
    
    reduced_matrix = reduce_matrix(full_matrix)
    
    print("####### MACIERZ ROZRÓŻNIALNOŚCI ZREDUKOWANA #######")
    for row in reduced_matrix:
        print(row)

    return reduced_matrix


def generate_diff_martrix(matrix):
    diff_matrix = []
    params = traits()

    for row in matrix:
        order = row[:len(row)-1]
        diff_row = []

        for row_compare in matrix:
            order_compare = row_compare[:len(row_compare)-1]
            diffs = []

            for idx_trait in range(len(order)):
                if order[idx_trait] != order_compare[idx_trait]:
                    diffs.append(params[idx_trait])
            
            diff_row.append(diffs)

        diff_matrix.append(diff_row)

    print("####### MACIERZ NIEROZRÓŻNIALNOŚCI #######")
    for idx_row, row in enumerate(diff_matrix):
        for idx_col, col in enumerate(row):
            print(f'{idx_row+1}/{idx_col+1}', col)
        
    return diff_matrix


def max_frequency_reduct(matrix):
    params = traits()
    freq = {}
    
    for param in params:
        freq[param] = 0

    max = 0
    answer = None

    for row in matrix:
        for col in row:
           for param in col:
               freq[param] += 1

               if freq[param] > max:
                   max = freq[param]
                   answer = param

    return answer



def del_reduct_from_matrix(reduct, matrix):
    result = []

    for row in matrix:
        row_to_add = []
        
        for col in row:
            if reduct not in col and len(col) > 0:
                row_to_add.append(col)

        if len(row_to_add) > 0:
            result.append(row_to_add)

    return result


def johnson_heuristics(matrix):
    reducts = []

    while len(matrix) > 0:
        a = max_frequency_reduct(matrix)
        reducts.append(a)
        matrix = del_reduct_from_matrix(a, matrix)
    
    print("####### REDUKTY #######")
    print(reducts)

    return reducts


def save_reducts(reducts):
    with open('redukty.pl', 'w') as f:
        for reduct in reducts:
            f.write(f'reduct({reduct}).' + '\n')


if __name__ == "__main__":
    matrix = make_matrix()
    diff_matrix = generate_diff_martrix(matrix)
    reducts = johnson_heuristics(diff_matrix)
    save_reducts(reducts)
