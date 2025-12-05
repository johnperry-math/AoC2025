pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Day05 is

   package IO renames Ada.Text_IO;

   type ID_Number is range 0 .. 2**64 - 1;

   package ID_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => ID_Number);
   subtype ID_Vector is ID_Vectors.Vector;

   Questionable_IDs : ID_Vector;

   package ID_IO is new IO.Integer_IO (Num => ID_Number);

   type ID_Range_Record is record
      Start, Finish : ID_Number;
   end record;

   package ID_Range_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => ID_Range_Record);
   subtype ID_Range_Vector is ID_Range_Vectors.Vector;

   ID_Ranges : ID_Range_Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      --  first read ranges
      loop
         declare
            Line          : constant String := IO.Get_Line (Input);
            Start, Finish : ID_Number;
            Position      : Positive;
         begin
            if Line'Length = 0 then
               exit;
            else
               ID_IO.Get (Line, Start, Position);
               ID_IO.Get (Line (Position + 2 .. Line'Last), Finish, Position);
               ID_Ranges.Append (ID_Range_Record'(Start, Finish));
            end if;
         end;
      end loop;
      --  now read items
      while not IO.End_Of_File (Input) loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            New_ID   : ID_Number;
            Position : Positive;
         begin
            ID_IO.Get (Line, New_ID, Position);
            Questionable_IDs.Append (New_ID);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   function Part_1 return Natural is
      Result : Natural := 0;
   begin
      for ID of Questionable_IDs loop
         for ID_Range of ID_Ranges loop
            if ID in ID_Range.Start .. ID_Range.Finish then
               Result := @ + 1;
               exit;
            end if;
         end loop;
      end loop;
      return Result;
   end Part_1;

   function Part_2 (Simplify_Me : ID_Range_Vector) return ID_Number is
      Simplified           : ID_Range_Vector;
      Simplified_Somewhere : Boolean := False;
      Simplified_New       : Boolean;
      Result               : ID_Number;
   begin
      for New_Range of Simplify_Me loop
         Simplified_New := False;
         for Old_Range of Simplified loop
            if Old_Range.Finish >= New_Range.Start
              and then New_Range.Finish >= Old_Range.Start
            then
               Old_Range.Start :=
                 ID_Number'Min (Old_Range.Start, New_Range.Start);
               Old_Range.Finish :=
                 ID_Number'Max (Old_Range.Finish, New_Range.Finish);
               Simplified_Somewhere := True;
               Simplified_New := True;
               exit;
            end if;
         end loop;
         if not Simplified_New then
            Simplified.Append (New_Range);
         end if;
      end loop;
      if Simplified_Somewhere then
         Result := Part_2 (Simplified);
      else
         Result := 0;
         for Each of Simplified loop
            Result := @ + Each.Finish - Each.Start + ID_Number (1);
         end loop;
      end if;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("There are" & Part_1'Image & " fresh ingredients");
   IO.Put_Line
     ("The ranges consider" & Part_2 (ID_Ranges)'Image & " ingredients fresh");
end Day05;
