using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ExpertInterface
{
    public class Illness : IDecision
    {
        public string name;

        public string mood;

        public string sleep_problems;

        public string suicidal_thoughts;

        public string visual_hallucinations;

        public string auditory_hallucinations;

        public string addictions;

        public string stress_intensity;

        public string sense_of_difference;

        public string change_in_body_weight;

        public string difficulty_focusing_attention;

        public string panic_attacks;

        public string lack_of_trust;

        public Illness(string name)
        {
            this.name = name;
        }

        public string GetDecision()
        {
            return name;
        }

        public string[] GetListOfAttributes()
        {
            return new string[]
            {
                mood,
                sleep_problems,
                suicidal_thoughts,
                visual_hallucinations,
                auditory_hallucinations,
                addictions,
                stress_intensity,
                sense_of_difference,
                change_in_body_weight,
                difficulty_focusing_attention,
                panic_attacks,
                lack_of_trust
            };
        }

        public string[] GetNamesOfAttributes()
        {
            return new string[]
            {
                "mood",
                "sleep_problems",
                "suicidal_thoughts",
                "visual_hallucinations",
                "auditory_hallucinations",
                "addictions",
                "stress_intensity",
                "sense_of_difference",
                "change_in_body_weight",
                "difficulty_focusing_attention",
                "panic_attacks",
                "lack_of_trust"
            };
        }

        public string GetRulePrefix()
        {
            return "illness_is";
        }

        public bool IsAttributeFuzzy(string attribute_name)
        {
            return attribute_name == "mood" || attribute_name == "stress_intensity" || attribute_name == "sense_of_difference" ||
                   attribute_name == "change_in_body_weight" || attribute_name == "lack_of_trust";
        }

        public void Display()
        {
            Console.WriteLine($"{mood}, {sleep_problems}, {suicidal_thoughts}, " +
                        $"{visual_hallucinations}, {auditory_hallucinations}, {addictions}, " +
                        $"{stress_intensity}, {sense_of_difference}, {change_in_body_weight}, " +
                        $"{difficulty_focusing_attention}, {panic_attacks}, {lack_of_trust}");
        }

        public bool IsIllnessEqual(Illness illness)
        {
            return mood == illness.mood && sleep_problems == illness.sleep_problems && suicidal_thoughts == illness.suicidal_thoughts &&
                visual_hallucinations == illness.visual_hallucinations && auditory_hallucinations == illness.auditory_hallucinations && addictions == illness.addictions &&
                stress_intensity == illness.stress_intensity && sense_of_difference == illness.sense_of_difference && change_in_body_weight == illness.change_in_body_weight &&
                difficulty_focusing_attention == illness.difficulty_focusing_attention && panic_attacks == illness.panic_attacks && lack_of_trust == illness.lack_of_trust;
        }
    }
}
