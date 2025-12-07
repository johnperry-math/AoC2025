#!/usr/bin/env dotnet

namespace day06
{

    public class Program
    {

        enum Operation
        {
            Sum,
            Product
        }

        class Problem_Record
        {
            public List<long> operands = [];
            public Operation operation;
        }

        static readonly List<Problem_Record> problems = [];

        static void Read_Input()
        {
            const string path = "../input.txt";
            try
            {
                using StreamReader reader = new(path);
                string? line;
                // first operand
                line = reader.ReadLine()
                ?? throw new Exception("unexpected end of file");
                string[] split = line.Split(
                    ' ',
                    StringSplitOptions.RemoveEmptyEntries
                );
                foreach (string entry in split)
                {
                    long value = long.Parse(entry);
                    Problem_Record problem = new();
                    problem.operands.Add(value);
                    problems.Add(problem);
                }
                // remaining operands
                foreach (int ith in Enumerable.Range(1, 3))
                {
                    line = reader.ReadLine()
                    ?? throw new Exception("unexpected end of file");
                    split = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                    foreach (int jth in Enumerable.Range(0, problems.Count))
                    {
                        problems[jth].operands.Add(long.Parse(split[jth]));
                    }
                }
                // operation
                line = reader.ReadLine()
                ?? throw new Exception("unexpected end of file");
                split = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                foreach (int ith in Enumerable.Range(0, problems.Count))
                {
                    if (split[ith] == "+")
                    {
                        problems[ith].operation = Operation.Sum;
                    }
                    else
                    {
                        problems[ith].operation = Operation.Product;
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"The file {path} could not be read:");
                Console.WriteLine(e.Message);
            }
        }

        static long Solution(List<Problem_Record> problems)
        {
            long result = 0;
            foreach (Problem_Record problem in problems)
            {
                switch (problem.operation)
                {
                    case Operation.Sum:
                        result += problem.operands
                            .Aggregate((acc, num) => acc + num);
                        break;
                    case Operation.Product:
                        result += problem.operands
                            .Aggregate((acc, num) => acc * num);
                        break;
                }
            }
            return result;
        }

        static List<Problem_Record> revised_problems = [];

        static void Reread_Input()
        {
            const string path = "../input.txt";
            try
            {
                string?[] number_lines = new string[4];
                string? operation_line;
                using StreamReader reader = new(path);
                // read in all the lines
                foreach (int ith in Enumerable.Range(0, 4))
                {
                    number_lines[ith] = reader.ReadLine()
                    ?? throw new Exception("unexpected end of file");
                }
                operation_line = reader.ReadLine()
                ?? throw new Exception("unexpected end of file");
                int position = 0;
                int problem_number = 0;
                while (position < operation_line.Length)
                {
                    int next_position = operation_line[(position + 1)..]
                                            .IndexOfAny(['+', '*']);
                    if (next_position < 0)
                    {
                        next_position = operation_line.Length - position;
                    }
                    int num_operands = next_position;
                    char[][] transposed_lines = new char[num_operands][];
                    foreach (int ith in Enumerable.Range(0, num_operands))
                    {
                        transposed_lines[ith] = new char[4];
                        foreach (var jth in Enumerable.Range(0, 4))
                        {
                            transposed_lines[ith][jth] = ' ';
                        }
                    }
                    foreach (int ith in Enumerable.Range(0, 4))
                    {
                        foreach (int jth in Enumerable.Range(0, num_operands))
                        {
                            transposed_lines[jth][ith]
                                = number_lines[ith]![position + jth];
                        }
                    }
                    Problem_Record new_problem = new()
                    {
                        operation = problems[problem_number].operation
                    };
                    problem_number += 1;
                    foreach (int ith in Enumerable.Range(0, num_operands))
                    {
                        string line = new(transposed_lines[ith]!);
                        long value = long.Parse(line);
                        new_problem.operands.Add(value);
                    }
                    revised_problems.Add(new_problem);
                    // + 1 because of offset when finding next_position
                    position += next_position + 1;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"The file {path} could not be read:");
                Console.WriteLine(e.Message);
            }
        }

        static void Main()
        {
            Read_Input();
            Console.WriteLine($"The sum of answers is {Solution(problems)}");
            Reread_Input();
            Console.WriteLine($"Read correctly, it's {Solution(revised_problems)}");
        }

    }
}
