using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ExpertInterface
{
    public class MinimalRules
    {
        List<IDecision> decisionTable;

        List<int>[][] matrix;

        string rulesFilename = "../../../../src/db_rules.pl";

        public MinimalRules(List<IDecision> rules)
        {
            decisionTable = rules;
        }

        public void CalculateRules()
        {
            CalculateMatrix();
            var rules = GetMinimalRules();
            SaveRulesToFile(rules, rulesFilename);
        }

        private void SaveRulesToFile(List<string> rules, string filename)
        {
            if (File.Exists(filename))
            {
                File.Delete(filename);
            }

            var module = ":- module(db_rules, [illness_is/1]). \n";

            var use_module = ":-use_module(engine_interface). \n";

            using (var streamWriter = new StreamWriter(filename))
            {
                streamWriter.WriteLine(module);
                streamWriter.WriteLine(use_module);

                foreach (var rule in rules)
                {
                    streamWriter.WriteLine(rule);
                }
            }
        }

        private List<int> FindDifferentIndexes(IDecision firstRule, IDecision secondRule)
        {
            var differendIdxs = new List<int>();
            var firstArguments = firstRule.GetListOfAttributes();
            var secondArguments = secondRule.GetListOfAttributes();
            for (int i = 0; i < firstArguments.Length && i < secondArguments.Length; i++)
            {
                if (firstArguments[i] != secondArguments[i])
                {
                    differendIdxs.Add(i);
                }
            }
            return differendIdxs;
        }
        private List<int>[][] CalculateMatrix()
        {
            int n = decisionTable.Count;

            matrix = new List<int>[n][];
            for (int i = 0; i < n; i++)
            {
                matrix[i] = new List<int>[n];
                for (int j = 0; j < n; j++)
                {
                    matrix[i][j] = new List<int>();
                }
            }

            for (int i = 0; i < n; i++)
            {
                var decision1 = decisionTable[i];
                for (int j = i + 1; j < n; j++)
                {
                    var decision2 = decisionTable[j];
                    if (decision1.GetDecision() != decision2.GetDecision())
                    {
                        var differendIdxs = FindDifferentIndexes(decision1, decision2);
                        matrix[i][j] = matrix[j][i] = differendIdxs;
                    }
                }
            }

            return matrix;
        }

        private List<List<int>> SimplifyRule(List<int>[] rule)
        {
            List<List<int>> rulesList = new List<List<int>>(rule.Where(r => r.Count != 0).OrderBy(x => x.Count));
            for (int i = 0; i < rulesList.Count; ++i)
            {
                List<int> indxs = new List<int>();
                for (int j = 0; j < rulesList.Count; ++j)
                {
                    if (i != j && rulesList[i].Intersect(rulesList[j]).Any())
                    {
                        indxs.Add(j);
                    }
                }

                foreach (var idx in indxs.OrderByDescending(x => x))
                {
                    rulesList.RemoveAt(idx);
                }
            }

            return rulesList;
        }

        private string GetMakeSure(string atr, string val)
        {
            return String.Format("make_sure({0}, {1})", atr, val);
        }

        private string CreateRule(IDecision decision, List<List<int>> simpleRule)
        {
            List<string> conditionsAND = new List<string>();

            var values = decision.GetListOfAttributes();
            var names = decision.GetNamesOfAttributes();

            foreach (var elements in simpleRule)
            {
                List<string> conditionsOR = new List<string>();

                foreach (var elem in elements)
                {
                    conditionsOR.Add(GetMakeSure(names[elem], values[elem]));
                }

                conditionsAND.Add("( \n" + String.Join(" ; \n", conditionsOR) + " \n)"); // OR
            }

            string ruleHeader = String.Format("{0}({1}) :- \n", decision.GetRulePrefix(), decision.GetDecision());

            string conditionsPart = String.Join(" , \n", conditionsAND);

            return ruleHeader + conditionsPart + ". \n"; ;
        }

        private List<string> GetMinimalRules()
        {
            List<string> minimalRules = new List<string>();
            for (int i = 0; i < matrix.Length; i++)
            {
                var simpleRule = SimplifyRule(matrix[i]);
                var rule = CreateRule(decisionTable[i], simpleRule);
                minimalRules.Add(rule);
            }

            return minimalRules;
        }
    }
}
