with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

procedure Day02 is

   package IO renames Ada.Text_IO;
   package Strings renames Ada.Strings.Fixed;

   package Math is new
     Ada.Numerics.Generic_Elementary_Functions (Float_Type => Float);

   type ID_Range is range 0 .. 2**64 - 1;

   type ID_Range_Record is record
      Start, Finish : ID_Range;
   end record;

   package ID_Range_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => ID_Range_Record);
   subtype ID_Range_Vector is ID_Range_Vectors.Vector;

   ID_Ranges : ID_Range_Vector;

   Debug    : constant Boolean := False;
   Filename : constant String :=
     (if Debug then "example.txt" else "input.txt");

   procedure Parse_Input (Line : String) is
      First_Position  : Positive := Line'First;
      Second_Position : Natural;
      Num_Ranges      : constant Positive := Strings.Count (Line, ",") + 1;
      New_ID          : ID_Range_Record;
   begin
      for Each in 1 .. Num_Ranges loop
         Second_Position := Strings.Index (Line, "-", First_Position);
         New_ID.Start :=
           ID_Range'Value (Line (First_Position .. Second_Position - 1));
         First_Position := Second_Position + 1;
         Second_Position := Strings.Index (Line, ",", First_Position);
         if Second_Position = 0 then
            Second_Position := Line'Last + 1;
         end if;
         New_ID.Finish :=
           ID_Range'Value (Line (First_Position .. Second_Position - 1));
         First_Position := Second_Position + 1;
         ID_Ranges.Append (New_ID);
      end loop;
   end Parse_Input;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, Filename);
      declare
         Line : constant String := IO.Get_Line (Input);
      begin
         Parse_Input (Line);
      end;
      IO.Close (Input);
   end Read_Input;

   function Num_Digits (Value : ID_Range) return Positive is
      Log10    : constant Float := Math.Log (10.0);
      Temp     : constant Float := Math.Log (Float (Value + 1)) / Log10;
      Int_Part : constant Natural := Natural (Float'Truncation (Temp));
   begin
      if Temp - Float (Int_Part) > 0.0 then
         return Int_Part + 1;
      else
         return Int_Part;
      end if;
   end Num_Digits;

   function Part_1 return ID_Range is
      Result : ID_Range := 0;
      Length : Positive;
   begin
      for Element of ID_Ranges loop
         for ID in Element.Start .. Element.Finish loop
            Length := Num_Digits (ID);
            if Length mod 2 = 0 then
               declare
                  ID_Image  : constant String := ID'Image;
                  As_String : constant String := ID_Image (2 .. ID_Image'Last);
               begin
                  if As_String (2 .. Length / 2 + 1)
                    = As_String (Length / 2 + 2 .. Length + 1)
                  then
                     Result := Result + ID;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      return Result;
   end Part_1;

   function Repeats_With_Length
     (Value : String; Sublength : Positive) return Boolean
   is
      Length        : constant Positive := Value'Length;
      Start, Finish : Positive;
   begin
      if Length mod Sublength /= 0 then
         return False;
      end if;
      for Multiple in 1 .. Length / Sublength - 1 loop
         Start := Multiple * Sublength + 2;
         Finish := (Multiple + 1) * Sublength + 1;
         if Value (Value'First .. Value'First + Sublength - 1)
           /= Value (Start .. Finish)
         then
            return False;
         end if;
      end loop;
      return True;
   end Repeats_With_Length;

   function Part_2 return ID_Range is
      Result : ID_Range := 0;
      Length : Positive;
   begin
      for Element of ID_Ranges loop
         for ID in Element.Start .. Element.Finish loop
            Length := Num_Digits (ID);
            for Sublength in 1 .. Length / 2 loop
               declare
                  ID_Image  : constant String := ID'Image;
                  As_String : constant String := ID_Image (2 .. ID_Image'Last);
               begin
                  if Repeats_With_Length (As_String, Sublength) then
                     Result := Result + ID;
                     IO.Put_Line ("found" & Id'Image & Sublength'Image);
                     exit;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put ("The sum of invalid id's is ");
   IO.Put_Line (Part_1'Image);
   IO.Put ("Upon further investigation, it's ");
   IO.Put_Line (Part_2'Image);
end Day02;
