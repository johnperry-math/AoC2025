#!/usr/bin/env dotnet

public class Program
{
    static readonly List<long> questionable_ids = [];

    class ID_Range_Record(long start, long finish)
    {
        public long start = start;
        public long finish = finish;
    }

    static readonly List<ID_Range_Record> id_ranges = [];

    static void Read_Input()
    {
        const string path = "../input.txt";
        try
        {
            using StreamReader reader = new(path);
            string? line = reader.ReadLine()
            ?? throw new Exception("unexpected end of file");

            while (line.Length > 0)
            {
                int position = line.IndexOf('-');
                long start = long.Parse(line[..position]);
                long finish = long.Parse(line[(position + 1)..]);
                id_ranges.Add(new(start, finish));
                line = reader.ReadLine()
                ?? throw new Exception("unexpected end of file");
            }
            while ((line = reader.ReadLine()) != null)
            {
                questionable_ids.Add(long.Parse(line));
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"The file {path} could not be read:");
            Console.WriteLine(e.Message);
        }
    }

    static int Part_1()
    {
        int result = 0;
        foreach (long id in questionable_ids)
        {
            foreach (ID_Range_Record range in id_ranges)
            {
                if (range.start <= id && id <= range.finish)
                {
                    result += 1;
                    break;
                }
            }
        }
        return result;
    }

    static long Part_2(List<ID_Range_Record> simplify_me)
    {
        List<ID_Range_Record> simplified = [];
        bool simplified_somewhere = false;

        foreach (ID_Range_Record new_range in simplify_me)
        {
            bool simplified_new = false;
            foreach (ID_Range_Record old_range in simplified)
            {
                if (
                    old_range.finish >= new_range.start
                    && new_range.finish >= old_range.start
                )
                {
                    old_range.start = Math.Min(
                        old_range.start,
                        new_range.start
                    );
                    old_range.finish = Math.Max(
                        old_range.finish,
                        new_range.finish
                    );
                    simplified_somewhere = true;
                    simplified_new = true;
                    break;
                }
            }
            if (!simplified_new)
            {
                simplified.Add(new_range);
            }
        }
        long result;
        if (simplified_somewhere)
        {
            result = Part_2(simplified);
        }
        else
        {
            result = 0;
            foreach (ID_Range_Record each in simplified)
            {
                result += each.finish - each.start + 1;
            }
        }
        return result;
    }

    static void Main()
    {
        Read_Input();
        Console.WriteLine($"There are {Part_1()} fresh ingredients");
        Console.WriteLine(
            $"The ranges consider {Part_2(id_ranges)} ingredients fresh"
        );
    }
}