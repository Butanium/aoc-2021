using AoC.Days;

namespace AoC.Utils;

public class Aoc
{
    public static readonly string? InputPath = System.Environment.GetEnvironmentVariable("AOC_INPUT_PATH");

    //TODO: Create a Day? type instead of returning null to let the parent function implement its own error handling mechanism
    public static IDay? GetDay(string day)
    {
        switch (day)
        {
            case "1":
                return new SonarSweep();
        }

        return null;
    }
}