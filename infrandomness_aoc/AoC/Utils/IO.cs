namespace AoC.Utils;

public class Io
{
    public static List<int> ReadFileIntoIntList(string filename)
    {
        var array = new List<int>();
        var path = Path.Combine(Aoc.InputPath + filename);
        string? line;

        if (!File.Exists(path))
        {
            throw new FileNotFoundException("input file :" + path + " not found. Please make sure the AOC_INPUT_PATH environment variable is set correctly.");
        }

        Stream stream = new FileStream(path, FileMode.Open);
        using var streamReader = new StreamReader(stream);
        while ((line = streamReader.ReadLine()) != null)
        {
            array.Add(int.Parse(line));
        }

        return array;
    }
}