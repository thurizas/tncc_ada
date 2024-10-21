

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Text_IO;                 use Ada.Text_IO;





package Body Lexer is

    procedure init(inputFile:Unbounded_string;flags:Byte) is
        -- F: File_Type;
    begin
        Open(F, In_File, To_String(inputFile));                        -- open input file

        --size : constant File_Size := Size(inputFile);
        -- TODO : get length of input file
        -- TODO : create a bounded_string to hold the text

        exception                                                      -- handle any exceptions
            when E: Name_Error =>

                raise PROGRAM_ERROR with Exception_Message(E);
        

    end init;

end Lexer;