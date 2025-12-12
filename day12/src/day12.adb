pragma Ada_2022;

with Ada.Text_IO;

procedure Day12 is

   package IO renames Ada.Text_IO;
   package Positive_IO is new IO.Integer_IO (Num => Positive);

   Number_Of_Gift_Shapes : constant Positive := 6;

   subtype Shape_Count_Range is Positive range 1 .. Number_Of_Gift_Shapes;
   Gift_Shape_Counts : array (Shape_Count_Range) of Positive;

   Input            : IO.File_Type;
   Result           : Natural := 0;
   This_Shape_Count : Natural;

begin
   IO.Open (Input, IO.In_File, "input.txt");
   for Ith in Shape_Count_Range loop
      This_Shape_Count := 0;
      for Jth in 1 .. 5 loop
         declare
            Line : String := IO.Get_Line (Input);
         begin
            if Jth in 2 .. 4 then
               for Kth in 1 .. 3 loop
                  if Line (Kth) = '#' then
                     This_Shape_Count := @ + 1;
                  end if;
               end loop;
            end if;
         end;
      end loop;
      Gift_Shape_Counts (Ith) := This_Shape_Count;
   end loop;
   while not IO.End_Of_File (Input) loop
      declare
         Width, Height     : Positive;
         Gift_Number       : Positive;
         Line              : String := IO.Get_Line (Input);
         Position          : Positive := Line'First;
         Tree_Spots_Needed : Natural := 0;
      begin
         Positive_IO.Get (Line, Width, Position);
         --  argh the colon
         Positive_IO.Get
           (Line (Position + 2 .. Position + 3), Height, Position);
         Position := @ + 3;
         for Ith in Shape_Count_Range loop
            Positive_IO.Get
              (Line (Position .. Line'Last), Gift_Number, Position);
            Position := @ + 2;
            Tree_Spots_Needed := @ + Gift_Number * Gift_Shape_Counts (Ith);
         end loop;
         if Tree_Spots_Needed <= Width * Height then
            Result := @ + 1;
         end if;
      end;
   end loop;
   IO.Close (Input);
   IO.Put_Line
     ("We can fit all of the presents listed in" & Result'Image & " regions");
end Day12;
