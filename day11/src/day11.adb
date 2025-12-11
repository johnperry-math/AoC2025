pragma Ada_2022;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;
--  with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Day11 is

   package IO renames Ada.Text_IO;

   Debug         : constant Boolean := False;
   type Example_Enum is (First, Second);
   Which_Example : constant Example_Enum := Second;

   use all type Ada.Containers.Count_Type;
   use all type Ada.Containers.Hash_Type;

   subtype Device is String (1 .. 3);

   --  package Device_Vectors is new
   --    Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Device);
   --  subtype Device_Vector is Device_Vectors.Vector;

   function Hash (Value : Device) return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type
         ((Character'Pos (Value (1)) - Character'Pos ('a'))
          * 26**2
          + (Character'Pos (Value (2)) - Character'Pos ('a')) * 26
          + (Character'Pos (Value (3)) - Character'Pos ('a'))));

   --  function "=" (Left, Right : Device_Set) return Boolean
   --  is (Left.Length = Right.Length
   --      and then (for all Ith in Left.First_Index .. Left.Last_Index =>
   --                  Left (Ith) = Right (Ith)));

   package Device_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Device,
        Hash                => Hash,
        Equivalent_Elements => "=");
   subtype Device_Set is Device_Sets.Set;

   function "=" (Left, Right : Device_Set) return Boolean
   renames Device_Sets."=";

   package Device_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Device,
        Element_Type    => Device_Set,
        Hash            => Hash,
        Equivalent_Keys => "=");
   subtype Device_Map is Device_Maps.Map;
   Cable_Map : Device_Map;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input,
         IO.In_File,
         (if Debug
          then
            (if Which_Example = First then "example.txt" else "example2.txt")
          else "input.txt"));
      while not IO.End_Of_File (Input) loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Src      : constant Device := Line (1 .. 3);
            Dests    : Device_Set;
            Position : Positive := Line'First + 5;
         begin
            while Position <= Line'Last loop
               Dests.Insert (Line (Position .. Position + 2));
               Position := @ + 4;
            end loop;
            Cable_Map.Insert (Src, Dests);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   type Part_Enum is (First, Second);

   type Bigun is range 0 .. 2**100 - 1;

   type Device_Pair_Record is record
      Source, Destination : Device;
   end record;

   function Hash (Pair : Device_Pair_Record) return Ada.Containers.Hash_Type
   is (Hash (Pair.Source) * 26**3 + Hash (Pair.Destination));

   function "=" (Left, Right : Device_Pair_Record) return Boolean
   is (Left.Source = Right.Source
       and then Left.Destination = Right.Destination);

   package Device_To_Natural_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Device_Pair_Record,
        Element_Type    => Bigun,
        Hash            => Hash,
        Equivalent_Keys => "=");
   subtype Device_To_Natural_Map is Device_To_Natural_Maps.Map;
   Cache : Device_To_Natural_Map;

   function DFS_It
     (New_Device, Goal : Device; Visited : in out Device_Set) return Bigun
   is
      Result : Bigun := 0;
   begin
      if New_Device = Goal then
         Result := 1;
      elsif (not Visited.Contains (New_Device))
        and then Cable_Map.Contains (New_Device)
      then
         Visited.Insert (New_Device);
         for Dest of Cable_Map (New_Device) loop
            if Cache.Contains ((Dest, Goal)) then
               Result := @ + Cache ((Dest, Goal));
            else
               declare
                  Temp : Bigun := DFS_It (Dest, Goal, Visited);
               begin
                  Cache.Insert ((Dest, Goal), Temp);
                  Result := @ + Temp;
               end;
            end if;
         end loop;
         Visited.Delete (New_Device);
      end if;
      return Result;
   end DFS_It;

   function Part_1 return Bigun is
      Empty_Path, Exclusions : Device_Set;
   begin
      return DFS_It ("you", "out", Empty_Path);
   end Part_1;

   function Part_2 return Bigun is
      Result, Temp           : Bigun := 1;
      Empty_Path, Exclusions : Device_Set;
   begin
      Temp := DFS_It ("dac", "out", Empty_Path);
      IO.Put_Line ("dac -> out?" & Temp'Image);
      if Temp /= 0 then
         Result := @ * Temp;
      end if;
      Temp := DFS_It ("fft", "dac", Empty_Path);
      IO.Put_Line ("fft -> dac?" & Temp'Image);
      if Temp /= 0 then
         Result := @ * Temp;
      end if;
      Temp := DFS_It ("dac", "fft", Empty_Path);
      IO.Put_Line ("dac -> fft?" & Temp'Image);
      if Temp /= 0 then
         Result := @ * Temp;
      end if;
      Temp := DFS_It ("svr", "fft", Empty_Path);
      IO.Put_Line ("svr -> fft?" & Temp'Image);
      if Temp /= 0 then
         Result := @ * Temp;
      end if;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("There are" & Part_1'Image & " paths from you to out");
   IO.Put_Line ("There are" & Part_2'Image & " paths from svr to out");
end Day11;
