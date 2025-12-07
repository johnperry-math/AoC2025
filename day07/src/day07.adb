pragma Ada_2022;

with Ada.Text_IO;

procedure Day07 is

   package IO renames Ada.Text_IO;

   type Feature_Enum is (Empty, Splitter);

   Columns : constant Positive := 141;
   Rows    : constant Positive := 142;

   subtype Col_Range is Positive range 1 .. Columns;
   subtype Row_Range is Positive range 1 .. Rows;

   Manifold : array (Row_Range, Col_Range) of Feature_Enum :=
     [others => [others => Empty]];

   type Position_Record is record
      Col : Col_Range;
      Row : Row_Range;
   end record;

   Start_Position : Position_Record;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      for Row in Row_Range loop
         declare
            Line : constant String := IO.Get_Line (Input);
         begin
            for Col in Col_Range loop
               case Line (Col) is
                  when '^'    =>
                     Manifold (Row, Col) := Splitter;

                  when 'S'    =>
                     Start_Position := (Col, Row);

                  when others =>
                     null;

               end case;
            end loop;
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   type Row_Set_Array is array (Col_Range) of Boolean;

   function Part_1 return Natural is
      Result     : Natural := 0;
      Curr, Next : Row_Set_Array;
   begin
      Curr := [others => False];
      Curr (Start_Position.Col) := True;
      for Row in Row_Range'First + 1 .. Row_Range'Last loop
         Next := [others => False];
         for Col in Col_Range when Curr (Col) loop
            if Manifold (Row - 1, Col) = Empty then
               Next (Col) := True;
            else
               Next (Col + 1) := True;
               Next (Col - 1) := True;
               Result := @ + 1;
            end if;
         end loop;
         Curr := Next;
      end loop;
      return Result;
   end Part_1;

   type Number is range 0 .. 2**64 - 1;

   type Row_Array is array (Col_Range) of Number;

   function Part_2 return Number is
      Curr, Next : Row_Array := [others => 0];
      Row        : Row_Range := Row_Range'First;
      Value      : Number;
   begin
      Curr (Start_Position.Col) := 1;
      while Row < Row_Range'Last loop
         Next := [others => 0];
         for Col in Col_Range when Curr (Col) > 0 loop
            Value := Curr (Col);
            if Manifold (Row, Col) = Empty then
               Next (Col) := @ + Value;
            else
               Next (Col - 1) := @ + Value;
               Next (Col + 1) := @ + Value;
            end if;
         end loop;
         Row := @ + 1;
         Curr := Next;
      end loop;
      return Curr'Reduce ("+", 0);
   end Part_2;

begin
   Read_Input;
   IO.Put_Line
     ("Starting at" & Start_Position.Row'Image & Start_Position.Col'Image);
   IO.Put_Line ("The beam splits" & Part_1'Image & " times");
   IO.Put_Line ("A single particle ends up on" & Part_2'Image & " timelines");
end Day07;
