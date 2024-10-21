with GNAT.Command_Line;           use GNAT.Command_Line;
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;    use Ada.Text_IO.Unbounded_IO;
with Ada.Exceptions;              use Ada.Exceptions;
with Interfaces;                  use Interfaces;
with lexer;                       use lexer;

PROCEDURE Main is
    subtype Byte is Unsigned_8;                                            -- flags is an 8-bit unsigned integer
    package Byte_IO is new Ada.Text_IO.Modular_IO(Byte);
    
    FLAGS_DEBUG  : constant Byte := 2#00000001#;                           -- debug flag
    FLAGS_LEX    : constant Byte := 2#00000010#;
    FLAGS_PAR    : constant Byte := 2#00000100#;
    FLAGS_CODEGEN: constant Byte := 2#00001000#;
    FLAGS_ALL    : constant Byte := 2#00001110#;

    procedure showVersion(name : Unbounded_String) is
      msg:Unbounded_String; 
    begin
      Append(msg, name);
      Append(msg, " - version 0.1.0");
      Put_Line(Item =>Ada.Strings.Unbounded.To_String(msg));
  
    end showVersion;

    procedure showHelp(name : Unbounded_String) is
      msg:Unbounded_String;
    begin
      Append(msg, name);                                      -- build first line
      Append(msg, " a tiny, nearly complete C compiler");
      Put_Line(Item =>Ada.Strings.Unbounded.To_String(msg));
      
      Delete(msg,1,Length(msg));                              -- delete the current message
      Append(msg, "Options:");                                -- build second line
      Append(msg, name);
      Append(msg, " [options] <input_file");
      Put_Line(Item=>Ada.Strings.Unbounded.To_String(msg));
      
      Put_Line("Options:");
      Put_Line(" -d                     debug mode");
      Put_Line("           --lex        only perform lexical analysis");
      Put_Line("           --par        onl perform upto parsing");
      Put_Line("           --codegen    only perform upto code generation");
      Put_Line(" -o <file>              specify output file name");
      Put_Line(" -v        --version    print version infomation and exit");
      Put_Line(" -h        --help       print this screen and exit");

  end showHelp;
 
BEGIN

  declare
    ch : Character := ' ';
    flags : Byte := FLAGS_ALL;
    inputFile : Unbounded_String := To_Unbounded_String("");    -- input file name
    outputFile : Unbounded_String := To_Unbounded_String("");   -- output file name
    
  begin
    loop
      case GetOpt(Switches => "d h v o: -lex -par -codegen -help -version") is
        when 'd' => 
          Put_Line("debug"); 
          flags := flags or FLAGS_DEBUG;
        
        when 'h' =>                                         -- call show usage function
          showHelp(To_Unbounded_String("tncc"));
          return;
          
        when 'o' =>                                         -- set output file name to parameter
          outputFile := To_Unbounded_String(Parameter);
          
        when 'v' =>                                         -- call show version function
          showVersion(To_Unbounded_String("tncc"));
          return;

        when '-' =>                                      -- deal with long switches
          if Full_Switch="-lex" then                        -- stop at lexical analysis
            flags := flags xor FLAGS_LEX;        
            Put_Line("only do lexical analysis:");
          elsif Full_Switch="-par" then                     -- stop at parsing
            flags := flags xor  FLAGS_PAR;                   
          elsif Full_Switch="-codegen" then                 -- stop at code generation
            flags := flags  xor FLAGS_CODEGEN;              
          elsif Full_Switch="-help" then  
            showHelp(To_Unbounded_String("tncc"));
          end if;
        when others => exit; 
      end case;
    end loop;  
    
    -- if output name is empty set output to a default name (a.out)
    if(Length(outputFile) = 0) then
      Append(outputFile, "a.out");
    end if;

    -- Tif input name is empty give error and exit.
    inputFile := To_Unbounded_String(Get_Argument);
    if(Length(inputFile) = 0) then
      raise PROGRAM_ERROR with "No input file found";
    end if;

    lexer.init(inputFile, flags);

    exception
        when E: Program_Error =>
          Put_Line(Exception_Message(E));
    

  end;
END Main;


