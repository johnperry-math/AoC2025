#!/usr/bin/env dotnet

public class Program
{

    const int DIMENSION = 140;
    static readonly bool[,] maze = new bool[DIMENSION, DIMENSION];

    static void Read_Input()
    {
        const string path = "../input.txt";
        try
        {
            using StreamReader reader = new(path);
            foreach (int row in Enumerable.Range(0, DIMENSION))
            {
                string? line = reader.ReadLine()
                    ?? throw new Exception("unexpected end of file");
                foreach (int col in Enumerable.Range(0, DIMENSION))
                {
                    maze[row, col] = line[col] == '@';
                }
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"The file {path} could not be read:");
            Console.WriteLine(e.Message);
        }
    }

    record Position_Record
    (
        int Row,
        int Col
    );

    static List<Position_Record> Accessible_TP()
    {
        List<Position_Record> result = [];
        // this is seriously some of the worst design ever
        // it's not enough that 1 doesn't enter the range
        // if i define it as -1, 1; it also doesn't enter the range
        // if i define it as -1, 2 !!!
        var offset_range = Enumerable.Range(-1, 3);
        // makesme worry if DIMENSION is also not going far enough...
        var dimension_range = Enumerable.Range(0, DIMENSION);
        foreach (int row in dimension_range)
        {
            foreach (int col in dimension_range)
            {
                if (maze[row, col])
                {
                    int neighbors = 0;
                    foreach (
                        int row_offset in offset_range.Where(
                            row_offset =>
                                dimension_range.Contains(row + row_offset)
                        )
                    )
                    {
                        foreach (
                            int col_offset in offset_range.Where(
                                col_offset =>
                                    dimension_range.Contains(col + col_offset)
                                    && (col_offset != 0 || row_offset != 0)
                            )
                        )
                        {
                            if (maze[row + row_offset, col + col_offset])
                            {
                                neighbors += 1;
                            }
                        }
                    }
                    if (neighbors < 4)
                    {
                        result.Add(new(row, col));
                    }
                }
            }
        }
        return result;
    }

    static uint Part_2()
    {
        uint result = 0;
        List<Position_Record> to_remove = Accessible_TP();

        while (to_remove.Count > 0)
        {
            result += (uint)to_remove.Count;
            foreach (Position_Record position in to_remove)
            {
                maze[position.Row, position.Col] = false;
            }
            to_remove = Accessible_TP();
        }
        return result;
    }

    public static void Main()
    {
        Read_Input();
        Console.WriteLine($"The forklift can access {Accessible_TP().Count} rolls");
        Console.WriteLine($"Altogether it can remove {Part_2()} rolls");
    }
}