// See https://aka.ms/new-console-template for more information

using AoC.Utils;

switch (args.Length)
{
    case 0:
        Console.WriteLine("Please enter as arguments the AoC day you want to execute.");
        break;
    case 1:
        Aoc.GetDay(args[0]).Run(args);
        break;
}