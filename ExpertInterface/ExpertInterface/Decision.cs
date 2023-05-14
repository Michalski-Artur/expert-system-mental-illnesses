using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ExpertInterface
{
    public interface IDecision
    {
        public string[] GetListOfAttributes();

        public string[] GetNamesOfAttributes();

        public string GetDecision();

        public bool IsAttributeFuzzy(string name);

        public string GetRulePrefix();

    }
}
