pragma Ada_2022;

with Ada.Containers.Generic_Array_Sort;
with Ada.Text_IO;

procedure Day08 is

   package IO renames Ada.Text_IO;

   Debug : constant Boolean := False;

   type Number is range 0 .. 2**54;
   package Number_IO is new Ada.Text_IO.Integer_IO (Num => Number);

   type Position_Record is record
      X, Y, Z : Number;
   end record;

   Number_Of_Boxes : constant Positive := (if Debug then 20 else 1_000);
   subtype Box_Range is Positive range 1 .. Number_Of_Boxes;
   Boxes           : array (Box_Range) of Position_Record;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input, IO.In_File, (if Debug then "example.txt" else "input.txt"));
      for Ith in Box_Range loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Char_Pos : Positive := Line'First;
         begin
            Number_IO.Get (Line, Boxes (Ith).X, Char_Pos);
            Number_IO.Get
              (Line (Char_Pos + 2 .. Line'Last), Boxes (Ith).Y, Char_Pos);
            Number_IO.Get
              (Line (Char_Pos + 2 .. Line'Last), Boxes (Ith).Z, Char_Pos);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   function Square_Distance (Left, Right : Position_Record) return Number
   is ((Left.X - Right.X)**2 + (Left.Y - Right.Y)**2 + (Left.Z - Right.Z)**2);

   Distances : array (Box_Range, Box_Range) of Number :=
     [others => [others => 0]];

   procedure Determine_Distances is
   begin
      for Ith in Box_Range loop
         for Jth in Ith + 1 .. Box_Range'Last loop
            Distances (Ith, Jth) := Square_Distance (Boxes (Ith), Boxes (Jth));
            Distances (Jth, Ith) := Distances (Ith, Jth);
         end loop;
      end loop;
   end Determine_Distances;

   type Assignment_Array is array (Box_Range) of Natural;

   type Boxes_To_Circuits is record
      Num_Circuits : Natural;
      Assignment   : Assignment_Array;
   end record;

   procedure Initialize_Circuits (Circuits : out Boxes_To_Circuits) is
   begin
      Circuits.Num_Circuits := 0;
      Circuits.Assignment := [others => 0];
   end Initialize_Circuits;

   type Connection_Record is record
      Ith, Jth : Box_Range;
      Distance : Number;
   end record;

   function "<" (Left, Right : Connection_Record) return Boolean is
      Result : Boolean;
   begin
      Result :=
        Left.Distance < Right.Distance
        or else (Left.Distance = Right.Distance
                 and then (Left.Ith < Right.Ith
                           or else (Left.Ith = Right.Ith
                                    and then Left.Jth < Right.Jth)));
      return Result;
   end "<";

   function Solution (Limited_To_Sought : Boolean := True) return Number is
      Circuits_Sought : constant Positive := (if Debug then 10 else 1_000);
      subtype Range_1000 is
        Natural
          range 1
                .. (if Limited_To_Sought
                    then Circuits_Sought
                    else Number_Of_Boxes * (Number_Of_Boxes - 1) / 2 + 1);
      type Distance_Array is array (Range_1000 range <>) of Connection_Record;
      procedure Sort_By_Distance is new
        Ada.Containers.Generic_Array_Sort
          (Index_Type   => Range_1000,
           Element_Type => Connection_Record,
           Array_Type   => Distance_Array);

      Connections        : Distance_Array (Range_1000);
      Circuits           : Boxes_To_Circuits;
      Distance           : Number;
      Max_Distance       : Number := 0;
      Ult_Conn, Pen_Conn : Natural := 0;

      procedure Identify_Shortest_Distances is
         Index_of_Max    : Natural := Range_1000'First;
         Num_Connections : Natural := Range_1000'First;

         procedure Identify_Max_And_Index is
         begin
            Max_Distance := 0;
            Index_of_Max := 0;
            for Ith in Range_1000 loop
               if Connections (Ith).Distance > Max_Distance then
                  Max_Distance := Connections (Ith).Distance;
                  Index_of_Max := Ith;
               end if;
            end loop;
         end Identify_Max_And_Index;

      begin
         for Ith in Box_Range loop
            for Jth in Ith + 1 .. Box_Range'Last loop
               Distance := Distances (Ith, Jth);
               if Num_Connections < Range_1000'Last then
                  Connections (Num_Connections) :=
                    Connection_Record'(Ith, Jth, Distance);
               elsif Num_Connections = Range_1000'Last then
                  Connections (Range_1000'Last) :=
                    Connection_Record'(Ith, Jth, Distance);
                  Identify_Max_And_Index;
               elsif Distance < Max_Distance then
                  Connections (Index_of_Max) :=
                    Connection_Record'(Ith, Jth, Distance);
                  Identify_Max_And_Index;
               end if;
               Num_Connections := @ + 1;
            end loop;
         end loop;
      end Identify_Shortest_Distances;

      procedure Connect_Shortest_1000 is
         Ith, Jth : Box_Range;
         Count    : Natural := 0;
      begin
         for Connection of Connections loop
            Count := @ + 1;
            Ith := Connection.Ith;
            Jth := Connection.Jth;
            Pen_Conn := Ith;
            Ult_Conn := Jth;
            if Circuits.Assignment (Ith) = 0
              and then Circuits.Assignment (Jth) = 0
            then
               Circuits.Num_Circuits := @ + 1;
               Circuits.Assignment (Ith) := Circuits.Num_Circuits;
               Circuits.Assignment (Jth) := Circuits.Num_Circuits;
            elsif Circuits.Assignment (Ith) = 0 then
               Circuits.Assignment (Ith) := Circuits.Assignment (Jth);
            elsif Circuits.Assignment (Jth) = 0 then
               Circuits.Assignment (Jth) := Circuits.Assignment (Ith);
            else
               declare
                  Destination : constant Natural :=
                    Natural'Min
                      (Circuits.Assignment (Ith), Circuits.Assignment (Jth));
                  Old_Ith     : constant Natural := Circuits.Assignment (Ith);
                  Old_Jth     : constant Natural := Circuits.Assignment (Jth);
               begin
                  for Kth in Box_Range loop
                     if Circuits.Assignment (Kth) = Old_Ith
                       or else Circuits.Assignment (Kth) = Old_Jth
                     then
                        Circuits.Assignment (Kth) := Destination;
                     end if;
                  end loop;
               end;
            end if;
            if (for all Ith in Box_Range => Circuits.Assignment (Ith) /= 0)
            then
               exit;
            end if;
         end loop;
      end Connect_Shortest_1000;

      function Prod_of_Lrgst_3 return Number is
         Sizes         : array (1 .. Circuits.Num_Circuits) of Number :=
           [others => 0];
         Big, Med, Sml : Number := 0;
      begin
         for Assignment of Circuits.Assignment when Assignment /= 0 loop
            Sizes (Assignment) := @ + 1;
         end loop;
         for Size of Sizes loop
            if Size >= Big then
               Sml := Med;
               Med := Big;
               Big := Size;
            elsif Size >= Med then
               Sml := Med;
               Med := Size;
            elsif Size > Sml then
               Sml := Size;
            end if;
         end loop;
         return Big * Med * Sml;
      end Prod_of_Lrgst_3;

   begin
      Initialize_Circuits (Circuits);
      Identify_Shortest_Distances;
      Sort_By_Distance (Connections);
      Connect_Shortest_1000;
      if Limited_To_Sought then
         return Prod_of_Lrgst_3;
      else
         return Boxes (Ult_Conn).X * Boxes (Pen_Conn).X;
      end if;
   end Solution;

begin
   Read_Input;
   Determine_Distances;
   IO.Put_Line ("Product of 3 largest circuits is" & Solution'Image);
   IO.Put_Line
     ("Product of last two connext x values is" & Solution (False)'Image);
end Day08;
