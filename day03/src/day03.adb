pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day03 is

   package IO renames Ada.Text_IO;
   package Natural_IO is new IO.Integer_IO (Num => Natural);

   Debug : constant Boolean := False;

   subtype Battery_String is String (1 .. (if Debug then 15 else 100));

   package Battery_String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Battery_String);
   subtype Battery_String_Vector is Battery_String_Vectors.Vector;

   Battery_Strings : Battery_String_Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input, IO.In_File, (if Debug then "example.txt" else "input.txt"));
      while not IO.End_Of_File (Input) loop
         Battery_Strings.Append (IO.Get_Line (Input));
      end loop;
      IO.Close (Input);
   end Read_Input;

   type Battery_Record is record
      Position : Positive;
      Joltage  : Natural;
   end record;

   function To_Natural (Joltage : Character) return Natural
   is (Character'Pos (Joltage) - Character'Pos ('0'));

   type Battery_Sequence is array (Positive range <>) of Battery_Record;

   function Find_Minimum (Sequence : Battery_Sequence) return Battery_Record is
      Result : Battery_Record := (Position => 1000, Joltage => 10);
   begin
      for Ith in Sequence'Range loop
         if Sequence (Ith).Joltage < Result.Joltage then
            Result.Joltage := Sequence (Ith).Joltage;
            Result.Position := Sequence (Ith).Position;
         end if;
      end loop;
      return Result;
   end Find_Minimum;

   function Can_Shift_To_Increase (Sequence : Battery_Sequence) return Natural
   is
   begin
      for Ith in 1 .. Sequence'Last - 1 loop
         if Sequence (Ith).Joltage < Sequence (Ith + 1).Joltage then
            return Ith;
         end if;
      end loop;
      return 0;
   end Can_Shift_To_Increase;

   type Joltage_Range is range 0 .. 2**64 - 1;

   function Solution (Length : Positive := 2) return Joltage_Range is
      Result : Joltage_Range := 0;
   begin
      for Battery_String of Battery_Strings loop
         declare
            Sequence : Battery_Sequence (1 .. Length) :=
              [for Ith in 1 .. Length =>
                 Battery_Record'
                   (Position => Ith,
                    Joltage  => To_Natural (Battery_String (Ith)))];
            Minimum  : Battery_Record := Find_Minimum (Sequence);
            Value    : Natural := 0;
            Joltage  : Joltage_Range := 0;
            Shifty   : Natural;
         begin
            for Ith in Sequence'Last + 1 .. Battery_String'Last loop
               Shifty := Can_Shift_To_Increase (Sequence);
               Value := To_Natural (Battery_String (Ith));
               if Shifty > 0 then
                  for Jth in Shifty .. Sequence'Last - 1 loop
                     Sequence (Jth) := Sequence (Jth + 1);
                  end loop;
                  Sequence (Sequence'Last) :=
                    Battery_Record'
                      (Position => Ith,
                       Joltage  => To_Natural (Battery_String (Ith)));
                  Minimum := Find_Minimum (Sequence);
               elsif Value > Minimum.Joltage then
                  for Jth in
                    Sequence'Range
                    when Sequence (Jth).Position >= Minimum.Position
                    and then Jth < Sequence'Last
                  loop
                     Sequence (Jth) := Sequence (Jth + 1);
                  end loop;
                  Sequence (Sequence'Last) :=
                    Battery_Record'
                      (Position => Ith,
                       Joltage  => To_Natural (Battery_String (Ith)));
                  Minimum := Find_Minimum (Sequence);
               end if;
            end loop;
            for Each of Sequence loop
               Joltage := @ * 10 + Joltage_Range (Each.Joltage);
            end loop;
            Result := @ + Joltage;
         end;
      end loop;
      return Result;
   end Solution;

begin
   Read_Input;
   IO.Put_Line ("Total output joltage is" & Solution'Image);
   IO.Put_Line ("With 12 batteries it's" & Solution (12)'Image);
end Day03;
