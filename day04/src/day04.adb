pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Day04 is

   package IO renames Ada.Text_IO;

   Debug     : constant Boolean := False;
   Dimension : constant Positive := (if Debug then 10 else 140);
   Filename  : constant String :=
     (if Debug then "example.txt" else "input.txt");

   subtype Dimension_Range is Integer range 1 .. Dimension;

   Maze : array (Dimension_Range, Dimension_Range) of Boolean :=
     [others => [others => False]];

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, Filename);
      for Row in Dimension_Range loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Col in Dimension_Range loop
               Maze (Row, Col) := Line (Col) = '@';
            end loop;
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   type Position_Record is record
      Row, Col : Dimension_Range;
   end record;

   package Position_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Position_Record);
   subtype Position_Vector is Position_Vectors.Vector;

   function Accessible_TP return Position_Vector is
      Result : Position_Vector;
   begin
      for Row in Dimension_Range loop
         for Col in Dimension_Range loop
            if Maze (Row, Col) then
               declare
                  Neighbors : Natural := 0;
               begin
                  for Row_Offset in
                    -1 .. 1
                    when Row + Row_Offset in Dimension_Range
                  loop
                     for Col_Offset in
                       -1 .. 1
                       when Col + Col_Offset in Dimension_Range
                       and then (Col_Offset /= 0 or else Row_Offset /= 0)
                     loop
                        if Maze (Row + Row_Offset, Col + Col_Offset) then
                           Neighbors := @ + 1;
                        end if;
                     end loop;
                  end loop;
                  if Neighbors < 4 then
                     Result.Append (Position_Record'(Row => Row, Col => Col));
                  end if;
               end;
            end if;
         end loop;
      end loop;
      return Result;
   end Accessible_TP;

   function Part_2 return Natural is
      Result    : Natural := 0;
      To_Remove : Position_Vector := Accessible_TP;
   begin
      while Natural (To_Remove.Length) > 0 loop
         Result := @ + Natural (To_Remove.Length);
         for Position of To_Remove loop
            Maze (Position.Row, Position.Col) := False;
         end loop;
         To_Remove := Accessible_TP;
      end loop;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put_Line
     ("The forklift can access" & Accessible_TP.Length'Image & " rolls");
   IO.Put_Line ("Altogether it can remove" & Part_2'Image & " rolls");
end Day04;
