pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Day06 is

   package IO renames Ada.Text_IO;

   subtype Line_Range is Positive range 1 .. 4;

   type Number is range 0 .. 2**64 - 1;

   package Number_IO is new IO.Integer_IO (Num => Number);

   --  type Operands_Array is array (Line_Range) of Number;
   type Operation_Enum is (Sum, Product);

   package Operands_Vectors is new
     Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Number);
   subtype Operands_Vector is Operands_Vectors.Vector;

   type Problem_Record is record
      Operands  : Operands_Vector;
      Operation : Operation_Enum;
   end record;

   package Problem_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Problem_Record);
   subtype Problem_Vector is Problem_Vectors.Vector;
   Problems : Problem_Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      --  first operand
      declare
         Line     : constant String := IO.Get_Line (Input);
         Position : Positive := Line'First;
         Value    : Number;
      begin
         while Position <= Line'Last loop
            Number_IO.Get (Line (Position .. Line'Last), Value, Position);
            Problems.Append
              (Problem_Record'(Operands => <>, Operation => Sum));
            Problems (Problems.Last_Index).Operands.Append (Value);
            Position := @ + 1;
         end loop;
      end;
      --  remaining operands
      for Ith in Line_Range'First + 1 .. Line_Range'Last loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Position : Positive := Line'First;
            Value    : Number;
         begin
            for Jth in 1 .. Natural (Problems.Length) loop
               Number_IO.Get (Line (Position .. Line'Last), Value, Position);
               Problems (Jth).Operands.Append (Value);
               Position := @ + 1;
            end loop;
         end;
      end loop;
      --  operation
      declare
         Line     : constant String := IO.Get_Line (Input);
         Position : Positive := Line'First;
      begin
         for Ith in 1 .. Natural (Problems.Length) loop
            while Line (Position) = ' ' loop
               Position := @ + 1;
            end loop;
            Problems (Ith).Operation :=
              (if Line (Position) = '+' then Sum else Product);
            Position := @ + 1;
         end loop;
      end;
      IO.Close (Input);
   end Read_Input;

   function Solution (Problem_Set : Problem_Vector) return Number is
      Result : Number := 0;
      Value  : Number;
   begin
      for Problem of Problem_Set loop
         case Problem.Operation is
            when Sum     =>
               Value := Problem.Operands'Reduce ("+", 0);

            when Product =>
               Value := Problem.Operands'Reduce ("*", 1);
         end case;
         Result := @ + Value;
      end loop;
      return Result;
   end Solution;

   Revised_Problems : Problem_Vector;

   procedure Reread_Input is
      Line_Length             : constant Positive := 3747;
      Number_Lines            :
        array (Line_Range) of String (1 .. Line_Length);
      Operation_Line          : String (1 .. Line_Length);
      Position, Next_Position : Positive;
      Problem_Number          : Positive := 1;
   begin
      --  read in all the lines
      declare
         Input : IO.File_Type;
      begin
         IO.Open (Input, IO.In_File, "input.txt");
         for Ith in Line_Range loop
            Number_Lines (Ith) := IO.Get_Line (Input);
         end loop;
         Operation_Line := IO.Get_Line (Input);
         IO.Close (Input);
      end;
      --  transpose the numbers, making it easier to parse
      Position := Operation_Line'First;
      while Position <= Operation_Line'Last loop
         Next_Position := Position + 1;
         while Next_Position <= Operation_Line'Last
           and then Operation_Line (Next_Position) = ' '
         loop
            Next_Position := @ + 1;
         end loop;
         --  add 1 for a phantom column separator
         if Next_Position > Operation_Line'Last then
            Next_Position := @ + 1;
         end if;
         declare
            Num_Operands     : constant Positive :=
              Next_Position - Position - 1;
            Transposed_Lines :
              array (1 .. Num_Operands) of String (Line_Range);
            New_Problem      : Problem_Record;
            Operand          : Number;
            Dummy            : Positive;
         begin
            for Ith in Line_Range loop
               for Jth in 1 .. Num_Operands loop
                  Transposed_Lines (Jth) (Ith) :=
                    Number_Lines (Ith) (Position + Jth - 1);
               end loop;
            end loop;
            New_Problem.Operation := Problems (Problem_Number).Operation;
            Problem_Number := @ + 1;
            for Line of Transposed_Lines loop
               Number_IO.Get (Line, Operand, Dummy);
               New_Problem.Operands.Append (Operand);
            end loop;
            Revised_Problems.Append (New_Problem);
         end;
         Position := Next_Position;
      end loop;
   end Reread_Input;

begin
   Read_Input;
   IO.Put_Line ("The sum of answers is" & Solution (Problems)'Image);
   Reread_Input;
   IO.Put_Line ("Read correctly, it's" & Solution (Revised_Problems)'Image);
end Day06;
