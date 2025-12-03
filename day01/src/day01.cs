#!/usr/bin/env dotnet

public class Program
{

    enum Turn { Left, Right };

    record InstructionRecord(Turn Direction, int Distance);

    static List<InstructionRecord> Get_Input()
    {
        const string path = "../input.txt";
        List<InstructionRecord> instructions = [];

        try
        {
            using (StreamReader reader = new(path))
            {
                string? line;
                while ((line = reader.ReadLine()) != null)
                {
                    instructions.Add(
                        new(
                            line[0] == 'L' ? Turn.Left : Turn.Right,
                            int.Parse(line.Substring(1, line.Length - 1))
                        )
                    );
                }
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"The file {path} could not be read:");
            Console.WriteLine(e.Message);
        }
        return instructions;
    }

    static int Part_1(List<InstructionRecord> instructions)
    {
        int result = 0;
        int position = 50;
        foreach (var instruction in instructions)
        {
            int distance = instruction.Distance;
            if (instruction.Direction == Turn.Left)
            {
                distance = -distance;
            }
            position = (position + distance) % 100;
            if (position == 0)
            {
                result += 1;
            }
        }
        return result;
    }

    static int Part_2(List<InstructionRecord> instructions)
    {
        int result = 0;
        int position = 50;
        int change, distance, cycles;

        foreach (var instruction in instructions)
        {
            distance = instruction.Distance;
            if (instruction.Direction == Turn.Left)
            {
                distance = -distance;
            }
            cycles = distance / 100;
            distance = distance - cycles * 100;
            result += Math.Abs(cycles);
            if (distance < 0)
            {
                while (distance != 0)
                {
                    if (position == 0 || distance > -position)
                    {
                        change = distance;
                    }
                    else
                    {
                        change = -position;
                    }
                    position += change;
                    distance -= change;
                    if (position < 0)
                    {
                        position += 100;
                    }
                    if (position == 0)
                    {
                        result += 1;
                    }
                }
            }
            else
            {
                while (distance != 0)
                {
                    if (position == 0 || distance < 100 - position)
                    {
                        change = distance;
                    }
                    else
                    {
                        change = 100 - position;
                    }
                    position += change;
                    distance -= change;
                    if (position > 99)
                    {
                        position -= 100;
                    }
                    if (position == 0)
                    {
                        result += 1;
                    }
                }
            }
        }
        return result;
    }

    public static void Main()
    {
        List<InstructionRecord> instructions = Get_Input();
        Console.WriteLine($"The password is {Part_1(instructions)}");
        Console.WriteLine($"No; it is {Part_2(instructions)}");
    }
}
