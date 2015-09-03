------------------------------------------------------------------------------
-- Copyright (c) 2013-2015, Natacha Porté                                   --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO.Text_Streams;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

with Natools.Getopt_Long;
with Natools.String_Slices;
with Markup.Parsers.Markdown.Extensions;

with Instances;

procedure Markdown is
   function Make_Parser return Markup.Parsers.Markdown.Markdown_Parser'Class;
   procedure Process_File (Filename : in String);
   procedure Process_Stream
     (Input : in out Ada.Streams.Root_Stream_Type'Class);


   package Options is
      type Action is (Error, Print_Help, Run);
      type Input_Format is (Official, Discount, Special_Extended);
      type Id is
        (Discount_Input,
         Help,
         Markdown_Input,
         Newline_Format,
         Html_Output,
         Special_Extended_Input,
         Xhtml_Output);

      package Getopt is new Natools.Getopt_Long (Id);

      function Config return Getopt.Configuration;

      function Value (Image : String)
        return Instances.Html_Stream.Newline_Format;

      type State is new Getopt.Handlers.Callback with record
         Action : Options.Action := Run;
         Arg_Count : Natural := 0;
         Output_Format : Instances.Html_Stream.Output_Format
           := Instances.Html_Stream.Html;
         Input_Format : Options.Input_Format := Official;
         Newline_Format : Instances.Html_Stream.Newline_Format
           := Instances.Html_Stream.Autodetect;
      end record;

      overriding procedure Option
        (Handler  : in out State;
         Id       : in Options.Id;
         Argument : in String);

      overriding procedure Argument
        (Handler  : in out State;
         Argument : in String);
   end Options;


   procedure Print_Help
     (Config : in Options.Getopt.Configuration;
      Output : in Ada.Text_IO.File_Type);


   package body Options is
      function Config return Getopt.Configuration is
         use Getopt;
         Result : Getopt.Configuration;
      begin
         Result.Add_Option ("discount",  'd', No_Argument, Discount_Input);
         Result.Add_Option ("help",      'h', No_Argument, Help);
         Result.Add_Option ("html",      'H', No_Argument, Html_Output);
         Result.Add_Option ("markdown",  'm', No_Argument, Markdown_Input);
         Result.Add_Option ("newline",   'n',
           Required_Argument, Newline_Format);
         Result.Add_Option ("nl",
           Required_Argument, Newline_Format);
         Result.Add_Option ("special",   's', No_Argument,
           Special_Extended_Input);
         Result.Add_Option ("xhtml",     'x', No_Argument, Xhtml_Output);
         return Result;
      end Config;


      overriding procedure Option
        (Handler  : in out State;
         Id       : in Options.Id;
         Argument : in String) is
      begin
         case Id is
            when Discount_Input =>
               Handler.Input_Format := Discount;
            when Help =>
               Handler.Action := Print_Help;
            when Html_Output =>
               Handler.Output_Format := Instances.Html_Stream.Html;
            when Markdown_Input =>
               Handler.Input_Format := Official;
            when Newline_Format =>
               begin
                  Handler.Newline_Format := Value (Argument);
               exception
                  when Constraint_Error =>
                     Handler.Action := Error;
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Current_Error,
                        "Unable to parse newline format """ & Argument & '"');
               end;
            when Special_Extended_Input =>
               Handler.Input_Format := Special_Extended;
            when Xhtml_Output =>
               Handler.Output_Format := Instances.Html_Stream.Xhtml;
         end case;
      end Option;


      overriding procedure Argument
        (Handler  : in out State;
         Argument : in String) is
      begin
         if Handler.Action = Run then
            Process_File (Argument);
            Handler.Arg_Count := Handler.Arg_Count + 1;
         end if;
      end Argument;


      function Value (Image : String)
        return Instances.Html_Stream.Newline_Format
      is
         function Is_Autodetect return Boolean;

         function Is_Autodetect return Boolean is
            Reconstructed : String := "autodetect";
            Length : constant Natural
              := Natural'Min (Image'Length, Reconstructed'Length);

            use type Instances.Html_Stream.Newline_Format;
         begin
            Reconstructed
              (Reconstructed'First .. Reconstructed'First + Length - 1)
              := Image (Image'First .. Image'First + Length - 1);
            return Instances.Html_Stream.Newline_Format'Value (Reconstructed)
              = Instances.Html_Stream.Autodetect;
         exception
            when Constraint_Error => return False;
         end Is_Autodetect;
      begin
         if Is_Autodetect then
            return Instances.Html_Stream.Autodetect;
         end if;

         case Image'Length is
            when 5 =>
               declare
                  Reconstructed : String (1 .. 5) := Image;
               begin
                  Reconstructed (3) := '_';
                  return Instances.Html_Stream.Newline_Format'Value
                    (Reconstructed);
               end;

            when 4 =>
               declare
                  Reconstructed : String (1 .. 5);
               begin
                  Reconstructed (1 .. 2)
                    := Image (Image'First .. Image'First + 1);
                  Reconstructed (3) := '_';
                  Reconstructed (4 .. 5)
                    := Image (Image'Last - 1 .. Image'Last);
                  return Instances.Html_Stream.Newline_Format'Value
                    (Reconstructed);
               end;

            when 2 =>
               return Instances.Html_Stream.Newline_Format'Value (Image);

            when others =>
               raise Constraint_Error;
         end case;
      end Value;
   end Options;


   Renderer : Instances.Html_Stream.Renderer_Ref;
   Opt : Options.State;


   function Make_Parser return Markup.Parsers.Markdown.Markdown_Parser'Class is
      use type Options.Input_Format;
   begin
      return Parser : Markup.Parsers.Markdown.Extensions.Extended_Parser do
         Parser.Atx_Header (Renderer.Header);
         Parser.Code_Block (Renderer.Code_Block);
         Parser.Horizontal_Rule (Renderer.Horizontal_Rule);
         Parser.Html_Block (Renderer.Raw_Html_Block);
         Parser.Html_Tag_Block (Renderer.Raw_Html_Block);
         Parser.Html_Comment_Block (Renderer.Raw_Html_Block);
         Parser.List (Renderer.Ordered_List, Renderer.List_Item,
                      Markup.Parsers.Markdown.Styles.Ordered);
         Parser.List (Renderer.Unordered_List, Renderer.List_Item,
                      Markup.Parsers.Markdown.Styles.Unordered);
         Parser.Paragraph (Renderer.Paragraph);
         Parser.Quote_Block (Renderer.Quote_Block);
         Parser.Setext_Header (Renderer.Header);

         Parser.Emphasis (Renderer.Emphasis, 1);
         Parser.Emphasis (Renderer.Strong, 2);
         Parser.Emphasis (Renderer.Strong_Emphasis, 3);
         Parser.Entity (Renderer.Raw_Html_Span);
         Parser.Escape (Renderer.Raw_Text_Span);
         Parser.Code_Span (Renderer.Code_Span);
         Parser.Discount_Image (Renderer.Image);
         Parser.Auto_Link (Renderer.Anchor);
         Parser.Html_Span (Renderer.Raw_Html_Span);

         if Opt.Input_Format < Options.Discount then
            return;
         end if;

         Parser.Discount_Definition_List
           (Renderer.Definition_List,
            Renderer.Definition_Title,
            Renderer.Definition_Description);
         Parser.PME_Definition_List
           (Renderer.Definition_List,
            Renderer.Definition_Title,
            Renderer.Definition_Description);
         Parser.PME_Table
           (Renderer.Table,
            Renderer.Table_Row,
            Renderer.Table_Header_Cell,
            Renderer.Table_Data_Cell);
         Parser.Discount_Centered (Renderer.Paragraph);
         Parser.Discount_Class_Block (Renderer.Division);
         Parser.Discount_Fenced_Code_Block (Renderer.Code_Block);

         Parser.Pseudoprotocol_Link (Renderer.Anchor);
         Parser.Pseudoprotocol_Abbr (Renderer.Abbreviation);
         Parser.Pseudoprotocol_Class (Renderer.Span);
         Parser.Pseudoprotocol_Id (Renderer.Anchor);
         Parser.Pseudoprotocol_Raw (Renderer.Raw_Html_Span);

         if Opt.Input_Format < Options.Special_Extended then
            return;
         end if;

         Parser.Paragraph_With_Class (Renderer.Paragraph, '(', ')');

         Parser.Emphasis (Renderer.Deleted, 2, "-");
         Parser.Emphasis (Renderer.Inserted, 2, "+");
         Parser.Emphasis (Renderer.Span, 1, "|");
      end return;
   end Make_Parser;


   procedure Print_Help
     (Config : in Options.Getopt.Configuration;
      Output : in Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      Indent : constant String := "    ";
   begin
      Put_Line (Output,
        "Usage: " & Ada.Command_Line.Command_Name
        & " [-h] [-H | -x] [-d | -m] [-n newline_format] [source_file]");
      New_Line (Output);
      Put_Line (Output, "Options:");

      for Id in Options.Id loop
         Put_Line (Output, Indent & Config.Format_Names (Id));

         case Id is
            when Options.Discount_Input =>
               Put_Line (Output, Indent & Indent
                 & "Enable Discount extensions in input");
            when Options.Help =>
               Put_Line (Output, Indent & Indent
                 & "Show this help text");
            when Options.Html_Output =>
               Put_Line (Output, Indent & Indent
                 & "Output HTML-style self-closing tags (e.g. <br>)");
            when Options.Markdown_Input =>
               Put_Line (Output, Indent & Indent
                 & "Parse input as strict official Markdown");
            when Options.Newline_Format =>
               Put_Line (Output, Indent & Indent
                 & "Set newline format.");
               Put_Line (Output, Indent & Indent
                 & "Allowed values: Autodetect, CR, LF, CR_LF, LF_CR");
            when Options.Special_Extended_Input =>
               Put_Line (Output, Indent & Indent
                 & "Enable Discount and special extensions in input");
            when Options.Xhtml_Output =>
               Put_Line (Output, Indent & Indent
                 & "Output XHTML-style self-closing tags (e.g. <br />)");
         end case;
      end loop;
   end Print_Help;


   procedure Process_File (Filename : in String) is
   begin
      if Filename = "-" then
         Process_Stream
           (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input).all);
      else
         declare
            File : Ada.Streams.Stream_IO.File_Type;
         begin
            Ada.Streams.Stream_IO.Open
              (File,
               Ada.Streams.Stream_IO.In_File,
               Filename);
            Process_Stream (Ada.Streams.Stream_IO.Stream (File).all);
            Ada.Streams.Stream_IO.Close (File);
         end;
      end if;
   end Process_File;


   procedure Process_Stream
     (Input : in out Ada.Streams.Root_Stream_Type'Class)
   is
      Text : Natools.String_Slices.Slice;
   begin
      Renderer.Set_Format (Opt.Output_Format);
      Renderer.Set_Newline (Opt.Newline_Format);

      Read_Text : declare
         use Ada.Streams;
         Chunk : Stream_Element_Array (1 .. 1024);
         Chunk_As_String : String (1 .. 1024);
         for Chunk_As_String'Address use Chunk'Address;
         Last : Stream_Element_Offset;
         Input_Text : Ada.Strings.Unbounded.Unbounded_String;
      begin
         loop
            Input.Read (Chunk, Last);
            exit when Last + 1 = Chunk'First;
            Ada.Strings.Unbounded.Append
              (Input_Text, Chunk_As_String (1 .. Positive (Last)));
            exit when Last < Chunk'Last;
         end loop;

         Text := Natools.String_Slices.To_Slice
           (Ada.Strings.Unbounded.To_String (Input_Text));
      end Read_Text;

      Process_Text : declare
         Parser : Markup.Parsers.Markdown.Markdown_Parser'Class := Make_Parser;
      begin
         Parser.Process (Text);
      end Process_Text;
   end Process_Stream;


   Config : constant Options.Getopt.Configuration := Options.Config;

   use type Options.Action;
begin
   Renderer.Set_Output (Ada.Text_IO.Text_Streams.Stream
     (Ada.Text_IO.Current_Output));

   Config.Process (Opt);

   if Opt.Arg_Count = 0 and Opt.Action = Options.Run then
      Process_File ("-");
   end if;

   case Opt.Action is
      when Options.Error =>
         Print_Help (Config, Ada.Text_IO.Current_Error);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      when Options.Print_Help =>
         Print_Help (Config, Ada.Text_IO.Current_Output);
      when Options.Run =>
         null;
   end case;
end Markdown;
