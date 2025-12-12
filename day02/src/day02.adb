with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
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

   function Smarter_1 return ID_Range is
      Result  : ID_Range := 0;
      Length  : Positive;
      Counter : ID_Range;
   begin
      for Element of ID_Ranges loop
         Length := Num_Digits (Element.Start);
         declare
            Start : constant String := Element.Start'Image;
         begin
            if Length mod 2 = 0 then
               Counter := ID_Range'Value (Start (2 .. Length / 2 + 1));
            else
               Counter := 10**(Length / 2);
            end if;
         end;
         loop
            declare
               Current       : constant String := Counter'Image;
               Doubled       : constant String :=
                 Current (2 .. Current'Length) & Current (2 .. Current'Length);
               Doubled_Value : constant ID_Range := ID_Range'Value (Doubled);
            begin
               exit when Doubled_Value > Element.Finish;
               Counter := Counter + 1;
               if Doubled_Value >= Element.Start then
                  Result := Result + Doubled_Value;
               end if;
            end;
         end loop;
      end loop;
      return Result;
   end Smarter_1;

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

   function Repeat (Substring : String; Repetitions : Positive) return String
   is
      Result   : String (1 .. Substring'Length * Repetitions) :=
        (others => ' ');
      Position : Positive := 1;
   begin
      while Position <= Result'Last loop
         Result (Position .. Position + Substring'Length - 1) := Substring;
         Position := Position + Substring'Length;
      end loop;
      return Result;
   end Repeat;

   function Smarter_2 return ID_Range is
      Result        : ID_Range := 0;
      Length        : Positive;
      function Hash (ID : ID_Range) return Ada.Containers.Hash_Type
      is (Ada.Containers.Hash_Type (ID mod 32003));
      package ID_Sets is new
        Ada.Containers.Hashed_Sets
          (Element_Type        => ID_Range,
           Hash                => Hash,
           Equivalent_Elements => "=");
      subtype ID_Set is ID_Sets.Set;
      Caught_Values : ID_Set;
   begin
      for Element of ID_Ranges loop
         Length := Num_Digits (Element.Finish);
         declare
            ID_Image       : constant String := Element.Start'Image;
            As_String      : constant String := ID_Image (2 .. ID_Image'Last);
            Stop_Image     : constant String := Element.Finish'Image;
            Stop_As_String : constant String :=
              Stop_Image (2 .. Stop_Image'Last);
         begin
            for Sublength in 1 .. Length / 2 loop
               declare
                  Power_Of_Ten : constant Positive := 10**(Sublength - 1);
                  Power_Image  : constant String := Power_Of_Ten'Image;
                  Power_String : constant String :=
                    Power_Image (2 .. Power_Image'Length);
                  Substring    : constant String :=
                    (if As_String'Length = Stop_As_String'Length
                     then As_String (2 .. Sublength + 1)
                     else Power_String);
                  Counter      : ID_Range := ID_Range'Value (Substring);
                  Stop_Counter : ID_Range :=
                    (if As_String'Length = Stop_As_String'Length
                     then ID_Range'Value (Stop_As_String (2 .. Sublength + 1))
                     else
                       ID_Range'Value (Stop_As_String (2 .. Sublength + 2)));
               begin
                  loop
                     for Repetitions in
                       Positive'Max (2, Num_Digits (Element.Start) / Sublength)
                       .. Num_Digits (Element.Finish) / Sublength
                     loop
                        declare
                           Counter_Image  : constant String := Counter'Image;
                           Counter_String : constant String :=
                             Counter_Image (2 .. Counter_Image'Length);
                           Repeated       : constant String :=
                             Repeat (Counter_String, Repetitions);
                           Repeated_Value : constant ID_Range :=
                             ID_Range'Value (Repeated);
                        begin
                           exit when Repeated_Value > Element.Finish;
                           if Repeated_Value >= Element.Start
                             and then not Caught_Values.Contains
                                            (Repeated_Value)
                           then
                              Result := Result + Repeated_Value;
                              Caught_Values.Include (Repeated_Value);
                           end if;
                        end;
                     end loop;
                     Counter := Counter + 1;
                     exit when Counter > Stop_Counter;
                  end loop;
               end;
            end loop;
         end;
      end loop;
      return Result;
   end Smarter_2;

   function Repeats_With_Length
     (Value : String; Sublength : Positive) return Boolean
   is
      Length        : constant Positive := Value'Length;
      Start, Finish : Positive;
   begin
      if Length mod Sublength /= 0 then
         return False;
      end if;
      --  ????!!!???
      --  ABC DEF GHI
      --  Length 9, Sublength 3
      --  Multiple 1
      --     Start = 1 * 3 + 2 = 5
      --     Finish = 2 * 3 + 1 = 7
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
   IO.Put_Line (Smarter_1'Image);
   --  IO.Put_Line (Part_1'Image);
   IO.Put ("Upon further investigation, it's ");
   IO.Put_Line (Smarter_2'Image);
--  IO.Put_Line (Part_2'Image);
end Day02;
