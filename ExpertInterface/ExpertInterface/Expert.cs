using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace ExpertInterface
{
    public class Expert
    {
        public List<Illness> illnesses;

        public Expert()
        {
            illnesses = new List<Illness>();
            InsertInitData();
        }

        public List<string> GetNames()
        {
            return illnesses.Select(i => i.name).Distinct().ToList();
        }

        private void InsertInitData()
        {
            using (var streamReader = new StreamReader("../../illnessesValues.txt"))
            {
                string line;
                while (!string.IsNullOrEmpty(line = streamReader.ReadLine()))
                {
                    var splitedLine = line.Split(',');
                    var illness = new Illness(splitedLine[0])
                    {
                        mood = splitedLine[1],
                        sleep_problems = splitedLine[2],
                        suicidal_thoughts = splitedLine[3],
                        visual_hallucinations = splitedLine[4],
                        auditory_hallucinations = splitedLine[5],
                        addictions = splitedLine[6],
                        stress_intensity = splitedLine[7],
                        sense_of_difference = splitedLine[8],
                        change_in_body_weight = splitedLine[9],
                        difficulty_focusing_attention = splitedLine[10],
                        panic_attacks = splitedLine[11],
                        lack_of_trust = splitedLine[12]
                    };

                    illnesses.Add(illness);
                }
            }
        }

        public void SaveIllnessesToFile()
        {
            if (File.Exists("../../illneseesValues.txt"))
            {
                File.Delete("../../illnessesValues.txt");
            }

            using (var streamWriter = new StreamWriter("../../illnessesValues.txt"))
            {
                foreach (var illness in illnesses)
                {
                    streamWriter.WriteLine($"{illness.name}, {illness.mood}, {illness.sleep_problems}, {illness.suicidal_thoughts}, " +
                        $"{illness.visual_hallucinations}, {illness.auditory_hallucinations}, {illness.addictions}, " +
                        $"{illness.stress_intensity}, {illness.sense_of_difference}, {illness.change_in_body_weight}, " +
                        $"{illness.difficulty_focusing_attention}, {illness.panic_attacks}, {illness.lack_of_trust}");

                }
            }
        }
    }
}
