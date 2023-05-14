using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Diagnostics;
using System.IO;

namespace ExpertInterface
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Choose type of program:");
            Console.WriteLine("1) recommender");
            Console.WriteLine("2) expert");
            var option = Console.ReadLine();
            while (option != "1" && option != "2")
            {
                Console.WriteLine("Invalid option specified.");
                Console.WriteLine("Possible options: 1, 2.");
                option = Console.ReadLine();
            }

            if (option == "1")
            {
                var expert = new Expert();
                var minRules = new MinimalRules(expert.illnesses.Select(x => (IDecision)x).ToList());
                minRules.CalculateRules();

                //recommender  - open prolog app
                Process myProcess = new Process();
                ProcessStartInfo myProcessStartInfo = new ProcessStartInfo("../../../../../swipl/bin/swipl-win.exe",
                    Path.GetFullPath("../../../../src/app.pl"));
                myProcessStartInfo.UseShellExecute = false;
                myProcessStartInfo.RedirectStandardOutput = true;
                myProcess.StartInfo = myProcessStartInfo;
                myProcess.Start();
            }
            else if (option == "2")
            {
                //expert
                var expert = new Expert();

                while (true)
                {
                    //Console.WriteLine();
                    //Console.WriteLine("What do you want to do?");
                    //Console.WriteLine("1) Insert illness");
                    //Console.WriteLine("2) Update illness");
                    //Console.WriteLine("3) Delete illness");
                    //option = Console.ReadLine();
                    //while (option != "1" && option != "2" && option != "3")
                    //{
                    //    Console.WriteLine("Invalid option specified.");
                    //    Console.WriteLine("Possible options: 1, 2, 3.");
                    //    option = Console.ReadLine();
                    //}

                    //if (option == "1") //insert
                    //{
                    //    var illnessName = GetIllnessName();

                    //    var newIllness = CreateUniqueIllness(illnessName, expert.illnesses);

                    //    expert.illnesses.Add(newIllness);
                    //    Console.WriteLine("Illness was inserted.");
                    //}
                    //else if (option == "2") //update
                    //{
                    //    Console.WriteLine("All illnesses:");
                    //    Console.WriteLine(String.Join(", ", expert.GetNames()));

                    //    var illnessToUpdate = GetIllness(expert.illnesses);
                    //    expert.illnesses.Remove(illnessToUpdate);

                    //    var newIllness = CreateUniqueIllness(illnessToUpdate.name, expert.illnesses);
                    //    expert.illnesses.Add(newIllness);

                    //    Console.WriteLine("Illness was updated!");
                    //}
                    //else if (option == "3") //delete
                    //{
                    //    Console.WriteLine("All illnesses:");
                    //    Console.WriteLine(String.Join(", ", expert.GetNames()));

                    //    var illnessToDelete = GetIllness(expert.illnesses);
                    //    expert.illnesses.Remove(illnessToDelete);

                    //    Console.WriteLine("Illness was succesfully deleted!");
                    //}

                    expert.SaveIllnessesToFile();
                    var minRules = new MinimalRules(expert.illnesses.Select(x => (IDecision)x).ToList());
                    minRules.CalculateRules();
                }
            }

            Console.ReadKey();
        }

        private static string GetIllnessName()
        {
            Console.WriteLine("Enter illness name:");
            var illnessName = Console.ReadLine();
            while (string.IsNullOrEmpty(illnessName))
            {
                Console.WriteLine("Illness name cannot be empty.");
                Console.WriteLine("Please specify name for the illness.");
                illnessName = Console.ReadLine();
            }

            return illnessName;
        }

        private static Illness GetIllness(List<Illness> allIllnesses)
        {
            var illnessName = GetIllnessName();
            var illnesses = allIllnesses.Where(i => i.name == illnessName);
            while (!illnesses.Any())
            {
                Console.WriteLine("No illnesses with specified name were found.");
                Console.WriteLine("Please try specifying another illness name.");
                illnessName = GetIllnessName();
                illnesses = allIllnesses.Where(i => i.name == illnessName);
            }

            int illnessToUpdateIndex = 1;
            if (illnesses.Count() > 1)
            {
                Console.WriteLine("These are illnesses with this name, which one do you want to update: (choose number)");

                int i = 1;
                foreach (var illness in illnesses)
                {
                    Console.Write(i++ + ") ");
                    illness.Display();
                }

                while (!int.TryParse(Console.ReadLine(), out illnessToUpdateIndex) ||
                    illnessToUpdateIndex <= 0 ||
                    illnessToUpdateIndex > illnesses.Count())
                {
                    Console.WriteLine("Error. Illness with this number does not exist!");
                    Console.WriteLine("Try specifying another number");
                }
            }

            return illnesses.ElementAt(illnessToUpdateIndex - 1);

        }

        private static Illness CreateUniqueIllness(string illnessName, List<Illness> allIllnesses)
        {
            var newIllness = CreateIllnessBasedOnReadValues(illnessName);
            while (allIllnesses.Any(i => i.IsIllnessEqual(newIllness)))
            {
                Console.WriteLine();
                Console.WriteLine("Cannot insert illness. Illness with those values already exists!");
                Console.WriteLine("Please specify values one more time.");
                Console.WriteLine();
                newIllness = CreateIllnessBasedOnReadValues(illnessName);
            }

            return newIllness;
        }

        public static Illness CreateIllnessBasedOnReadValues(string illnessName)
        {
            var newIllness = new Illness(illnessName);

            Console.WriteLine("Enter mood:");
            Console.WriteLine("Available options are: low, medium, high");
            var value = Console.ReadLine();
            while (value != "low" && value != "medium" && value != "high")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: low, medium, high");
                value = Console.ReadLine();
            }
            newIllness.mood = value;

            Console.WriteLine("Enter sleep problems:");
            Console.WriteLine("Available options are: yes, no");
            value = Console.ReadLine();
            while (value != "yes" && value != "no")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: yes, no");
                value = Console.ReadLine();
            }
            newIllness.sleep_problems = value;

            Console.WriteLine("Enter suicidal thoughts:");
            Console.WriteLine("Available options are: yes, no");
            value = Console.ReadLine();
            while (value != "yes" && value != "no")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: yes, no");
                value = Console.ReadLine();
            }
            newIllness.suicidal_thoughts = value;

            Console.WriteLine("Enter visual hallucinations:");
            Console.WriteLine("Available options are: no, rarely, often");
            value = Console.ReadLine();
            while (value != "no" && value != "rarely" && value != "often")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: no, rarely, often");
                value = Console.ReadLine();
            }
            newIllness.visual_hallucinations = value;

            Console.WriteLine("Enter auditory hallucinations:");
            Console.WriteLine("Available options are: no, rarely, often");
            value = Console.ReadLine();
            while (value != "no" && value != "rarely" && value != "often")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: no, rarely, often");
                value = Console.ReadLine();
            }
            newIllness.auditory_hallucinations = value;

            Console.WriteLine("Enter addictions:");
            Console.WriteLine("Available options are: yes, no");
            value = Console.ReadLine();
            while (value != "yes" && value != "no")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: yes, no");
                value = Console.ReadLine();
            }
            newIllness.addictions = value;

            Console.WriteLine("Enter stress intensity:");
            Console.WriteLine("Available options are: low, medium, high");
            value = Console.ReadLine();
            while (value != "low" && value != "medium" && value != "high")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: low, medium, high");
                value = Console.ReadLine();
            }
            newIllness.stress_intensity = value;

            Console.WriteLine("Enter sense of difference:");
            Console.WriteLine("Available options are: low, medium, high");
            value = Console.ReadLine();
            while (value != "low" && value != "medium" && value != "high")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: low, medium, high");
                value = Console.ReadLine();
            }
            newIllness.sense_of_difference = value;

            Console.WriteLine("Enter change in body weight:");
            Console.WriteLine("Available options are: low, medium, high");
            value = Console.ReadLine();
            while (value != "low" && value != "medium" && value != "high")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: low, medium, high");
                value = Console.ReadLine();
            }
            newIllness.change_in_body_weight = value;

            Console.WriteLine("Enter difficulty in focusing attention:");
            Console.WriteLine("Available options are: yes, no");
            value = Console.ReadLine();
            while (value != "yes" && value != "no")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: yes, no");
                value = Console.ReadLine();
            }
            newIllness.difficulty_focusing_attention = value;

            Console.WriteLine("Enter panic attack:");
            Console.WriteLine("Available options are: yes, no");
            value = Console.ReadLine();
            while (value != "yes" && value != "no")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: yes, no");
                value = Console.ReadLine();
            }
            newIllness.panic_attacks = value;

            Console.WriteLine("Enter lack of trust:");
            Console.WriteLine("Available options are: low, medium, high");
            value = Console.ReadLine();
            while (value != "low" && value != "medium" && value != "high")
            {
                Console.WriteLine("This option is not available!");
                Console.WriteLine("Available options are: low, medium, high");
                value = Console.ReadLine();
            }
            newIllness.lack_of_trust = value;

            return newIllness;
        }
    }
}
