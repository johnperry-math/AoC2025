#!/usr/bin/env dotnet

namespace day07
{
    public class Program
    {

        enum Feature_Enum
        {
            Empty,
            Splitter
        }

        const int COLUMNS = 141;
        const int ROWS = 142;

        static Feature_Enum[,] manifold = new Feature_Enum[ROWS, COLUMNS];

        record Position_Record(int Col, int Row);

        static int start_position;

        static void Read_Input()
        {
            const string path = "../input.txt";
            try
            {
                using StreamReader reader = new(path);
                string? line;

                foreach (int row in Enumerable.Range(0, ROWS))
                {
                    line = reader.ReadLine()
                    ?? throw new Exception("unexpected end of file");
                    foreach (int col in Enumerable.Range(0, COLUMNS))
                    {
                        if (line[col] == '^')
                        {
                            manifold[row, col] = Feature_Enum.Splitter;
                        }
                        else
                        {
                            manifold[row, col] = Feature_Enum.Empty;
                            if (line[col] == 'S')
                            {
                                start_position = col;
                            }
                        }

                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"The file {path} could not be read:");
                Console.WriteLine(e.Message);
            }
        }

        static uint Part_1()
        {
            uint result = 0;
            bool[] curr = [.. Enumerable.Repeat(false, COLUMNS)];
            curr[start_position] = true;

            foreach (int row in Enumerable.Range(1, ROWS - 1))
            {
                bool[] next = [.. Enumerable.Repeat(false, COLUMNS)];
                foreach (int col in Enumerable.Range(0, COLUMNS)
                    .Where(col => curr[col]))
                {
                    if (manifold[row - 1, col] == Feature_Enum.Empty)
                    {
                        next[col] = true;
                    }
                    else
                    {
                        next[col + 1] = true;
                        next[col - 1] = true;
                        result += 1;
                    }
                }
                curr = next;
            }
            return result;
        }

        static ulong Part_2()
        {
            ulong[] curr = [.. Enumerable.Repeat((ulong)0, COLUMNS)];
            curr[start_position] = 1;
            foreach (int row in Enumerable.Range(0, ROWS - 1))
            {
                ulong[] next = [.. Enumerable.Repeat((ulong)0, COLUMNS)];
                foreach (int col in Enumerable.Range(0, COLUMNS))
                {
                    ulong value = curr[col];
                    if (manifold[row, col] == Feature_Enum.Empty)
                    {
                        next[col] += value;
                    }
                    else
                    {
                        next[col - 1] += value;
                        next[col + 1] += value;
                    }
                }
                curr = next;
            }
            return curr.Aggregate((acc, value) => acc + value);
        }

        static void Main()
        {
            Read_Input();
            Console.WriteLine($"Starting at 1 {start_position}");
            Console.WriteLine($"The beam splits {Part_1()} times");
            Console.WriteLine($"A single particle ends up on {Part_2()} timelines");
        }

    }
}