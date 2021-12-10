using AoC.Utils;

namespace AoC.Days;

public class SonarSweep : IDay
{

    private static int ProcessIncremental(List<int> numbers)
    {
        int? number = null;
        var count = 0;
        foreach (var t in numbers)
        {
            if (number < t)
            {
                count++;
            }

            number = t;
        }

        return count;
    }

    private static int ProcessIncrementalPartTwo(List<int> numbers)
    {
        int? number = null;
        var count = 0;
        
        foreach (var t in numbers)
        {
            for (var i = 1; i < numbers.Count; i++)
            {
                for (var j = 2; j < numbers.Count; j++)
                {
                    if (number < t + numbers[i] + numbers[j])
                    {
                        count++;
                    }

                    number = t + numbers[i] + numbers[j];
                }

            }
        }
        return count;
    }
    private static void SolvePartOne(List<int> content)
    {
        Console.WriteLine("=== Part One ===");
        Console.WriteLine("Number of increments : {0}", ProcessIncremental(content));
    }

    private static void SolvePartTwo(List<int> content)
    {
        Console.WriteLine("=== Part Two ===");
        Console.WriteLine("Number of increments : {0}", ProcessIncrementalPartTwo(content));
    }

    public void Run(string[] arguments)
    {
        var content = Io.ReadFileIntoIntList("day1");
        SolvePartOne(content);
        SolvePartTwo(content);
    }
}