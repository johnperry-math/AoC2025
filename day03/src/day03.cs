#!/usr/bin/env dotnet

using System.Net;

public class Program
{

    // const int LENGTH = 15;
    const int LENGTH = 100;
    static List<int[]> battery_strings = [];

    static void Read_Input()
    {
        // const string path = "../example.txt";
        const string path = "../input.txt";
        try
        {
            string? line;
            using StreamReader reader = new(path);
            while ((line = reader.ReadLine()) != null)
            {
                int[] new_string = new int[LENGTH];
                var chars = line.ToCharArray();
                foreach (int ith in Enumerable.Range(0, LENGTH))
                {
                    new_string[ith] = line[ith] - '0';
                }
                battery_strings.Add(new_string);
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"The fine {path} could not be read:");
            Console.WriteLine(e.Message);
        }
    }

    struct Battery_Record(int position, int joltage)
    {
        public int position = position;
        public int joltage = joltage;
    };

    static Battery_Record Find_Minimum(Battery_Record[] sequence)
    {
        Battery_Record result = new(1000, 10);
        for (int ith = 0; ith < sequence.Length; ++ith)
        {
            if (sequence[ith].joltage < result.joltage)
            {
                result.joltage = sequence[ith].joltage;
                result.position = sequence[ith].position;
            }
        }
        return result;
    }

    static int Can_Shift_To_Increase(Battery_Record[] sequence)
    {
        for (int ith = 0; ith < sequence.Length - 1; ++ith)
        {
            if (sequence[ith].joltage < sequence[ith + 1].joltage)
            {
                return ith;
            }
        }
        return -1;
    }

    static long Solution(int length = 2)
    {
        long result = 0;
        foreach (int[] battery_string in battery_strings)
        {
            Battery_Record[] sequence = new Battery_Record[length];
            for (int ith = 0; ith < length; ++ith)
            {
                sequence[ith] = new Battery_Record(ith, battery_string[ith]);
            }
            Battery_Record minimum = Find_Minimum(sequence);
            int value = 0;
            long joltage = 0;
            int shifty;
            for (int ith = sequence.Length; ith < LENGTH; ++ith)
            {
                shifty = Can_Shift_To_Increase(sequence);
                value = battery_string[ith];
                if (shifty > -1)
                {
                    for (int jth = shifty; jth < length - 1; ++jth)
                    {
                        sequence[jth] = sequence[jth + 1];
                    }
                    sequence[length - 1] = new Battery_Record(
                        ith,
                        battery_string[ith]
                    );
                    minimum = Find_Minimum(sequence);
                }
                else if (value > minimum.joltage)
                {
                    foreach (
                        int jth in Enumerable.Range(0, length).Where(
                            jth => sequence[jth].position >= minimum.position
                            && jth < length - 1
                        )
                    )
                    {
                        sequence[jth] = sequence[jth + 1];
                    }
                    sequence[length - 1] = new Battery_Record(
                        ith,
                        battery_string[ith]
                    );
                    minimum = Find_Minimum(sequence);
                }
            }
            foreach (Battery_Record battery in sequence)
            {
                joltage = joltage * 10 + battery.joltage;
            }
            result += joltage;
        }
        return result;
    }

    static public void Main()
    {
        Read_Input();
        Console.WriteLine($"Total output joltage is {Solution()}");
        Console.WriteLine($"With 12 batteries it's {Solution(12)}");
    }

}