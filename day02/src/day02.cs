#!/usr/bin/env dotnet

using System.Collections;

public class Program
{

    record ID_Range_Record(
        ulong Start,
        ulong Finish
    );

    static List<ID_Range_Record> Parse_Input(string line)
    {
        List<ID_Range_Record> result = [];
        int first_position = 0;
        int second_position;
        int num_ranges = line.Count(',') + 1;
        ID_Range_Record new_id;

        foreach (int i in Enumerable.Range(1, num_ranges))
        {
            second_position = line[first_position..].IndexOf('-');
            string start_string = line[
                first_position..(first_position + second_position)
            ];
            ulong start = ulong.Parse(start_string);
            first_position += second_position + 1;
            second_position = line[first_position..].IndexOf(',');
            if (second_position < 0)
            {
                second_position = line.Length - first_position;
            }
            string finish_string = line[
                first_position..(first_position + second_position)
            ];
            ulong finish = ulong.Parse(finish_string);
            new_id = new(Start: start, Finish: finish);
            first_position += second_position + 1;
            result.Add(new_id);
        }
        return result;
    }

    static List<ID_Range_Record> Read_Input()
    {
        const string path = "../input.txt";
        try
        {
            using StreamReader reader = new(path);
            string? line = reader.ReadLine()
                ?? throw new Exception("received an empty input.txt?!?");
            return Parse_Input(line);
        }
        catch (Exception e)
        {
            Console.WriteLine($"The fine {path} could not be read:");
            Console.WriteLine(e.Message);
        }
        return [];
    }

    static int Num_Digits(ulong value)
    {
        double temp = Math.Log10(value);
        int int_part = (int)Math.Floor(temp);
        if (temp == 0 || temp - int_part > 0.0)
        {
            return int_part + 1;
        }
        else
        {
            return int_part;
        }
    }

    class UIntEnumerable : IEnumerable<ulong>
    {

        private UIntEnumerator enumerator;

        public UIntEnumerable(ulong start, ulong finish)
        {
            enumerator = new(start, finish);
        }

        public IEnumerator<ulong> GetEnumerator()
        {
            return enumerator;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

    class UIntEnumerator : IEnumerator<ulong>
    {

        private ulong current, start_from, finish_at;
        public ulong Current { get { return current - 1; } }

        public UIntEnumerator(ulong start, ulong finish)
        {
            current = start_from = start;
            finish_at = finish;
        }

        object IEnumerator.Current => Current;

        public void Dispose()
        {
        }

        public bool MoveNext()
        {
            if (current > finish_at)
            {
                return false;
            }
            else
            {
                current += 1;
                return true;
            }
        }

        public void Reset()
        {
            current = start_from - 1;
        }
    }

    static ulong Part_1(List<ID_Range_Record> id_ranges)
    {
        ulong result = 0;
        foreach (ID_Range_Record element in id_ranges)
        {
            UIntEnumerable range = new(element.Start, element.Finish);
            foreach (ulong id in range)
            {
                int length = Num_Digits(id);
                if (length % 2 == 0)
                {
                    string as_string = id.ToString();
                    if (
                        as_string[0..(length / 2)]
                        == as_string[(length / 2)..length]
                    )
                    {
                        result += id;
                    }
                }
            }
        }
        return result;
    }

    static bool Repeats_With_Length(string value, int sublength)
    {
        int length = value.Length;
        if (length % sublength != 0 || length == sublength)
        {
            return false;
        }
        foreach (int multiple in Enumerable.Range(1, length / sublength - 1))
        {
            int start = multiple * sublength;
            int finish = (multiple + 1) * sublength;
            if (value[0..sublength] != value[start..finish])
            {
                return false;
            }
        }
        return true;
    }

    static ulong Part_2(List<ID_Range_Record> ids)
    {
        ulong result = 0;
        foreach (ID_Range_Record element in ids)
        {
            UIntEnumerable range = new(element.Start, element.Finish);
            foreach (ulong id in range)
            {
                int length = Num_Digits(id);
                foreach (int sublength in Enumerable.Range(1, length / 2 + 1))
                {
                    string as_string = id.ToString();
                    if (Repeats_With_Length(as_string, sublength))
                    {
                        result += id;
                        break;
                    }
                }
            }
        }
        return result;
    }

    public static void Main()
    {
        List<ID_Range_Record> id_ranges = Read_Input();
        Console.WriteLine($"The sum of invalid id's is {Part_1(id_ranges)}");
        Console.WriteLine(
            $"Upon further investigation, it's {Part_2(id_ranges)}"
        );
    }
}