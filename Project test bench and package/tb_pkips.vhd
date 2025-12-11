-- tb_npkips_mem.vhd EGRE 426 Fall 09 Version 1.0
-----------------------------------------------------------------------
--  Test bench for pipelined KIPS processor
--  The test asm program is placed in m_in.txt
--  Load_mem reads the file m_in.txt into memory array "IMEM". 
--  The procedure dump_mem writes the contents of "DMEM" to the 
--  file m_out.txt after 5 us.
-----------------------------------------------------------------------
LIBRARY IEEE;
USE work.all;
USE IEEE.Std_Logic_1164.all; 
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL; 
USE std.textio.all;
use ieee.std_logic_textio.all;
use pkg_pkips.all;     -- Contains KIPS definitions
ENTITY TB IS END ENTITY TB;

ARCHITECTURE TEST OF TB IS
    Type Inst_type is (J, BEQ, ADD, SUB, iAND, iXOR, iNOR, LW, SW, ADDI, ANDI, 
                       ORI, XORI, SLTI, iOR, SLT, JR, LUI, JAL, BAD);
    SIGNAL inst: inst_type;
    SUBTYPE Word IS STD_LOGIC_VECTOR(WordSize - 1 downto 0);
    type MEM_array is array (natural range 0 to MemSize-1) of Word;
    SIGNAL RES, CLK: std_logic := '0';
    SIGNAL caddr, im: std_logic_vector(31 downto 0); -- Code addr and data
    SIGNAL daddr, dm: std_logic_vector(31 downto 0); -- Data memory addr and data   
    SIGNAL iaddr, idaddr: integer range 0 to MemSize - 1; -- Integer addr for code and data
    signal nIR:     std_logic := '1';
    signal nIW:     std_logic := '1';
    signal nRD:     std_logic := '1';
    signal nWR:     std_logic := '1';
    SIGNAL IMEM: mem_array;  -- Code memory
    SIGNAL DMEM: mem_array;  -- Data memory
    signal OP, FUNC: std_logic_vector(5 downto 0);
BEGIN
    CPU: ENTITY work.pkips(arch)
        PORT MAP
        (RES => Res,
         CLK => CLK,
         CADDR => CADDR,
         DADDR => DADDR,
         IM => IM,
         DM => DM,
         nIR => nIR,
         nRD => nRD,
         nWR => nWR
        );
-- Generate reset and clock stimulus        
    RES <= '1', '0' after 10 ns;
    CLK <= '1' when RES = '1' else not CLK after 10 ns;
-- Translate instruction and data addresses into 
-- index pointers for IMEM and IDATA array
    iaddr <= CONV_INTEGER(caddr - CS_ADDR)/4 when caddr >= CS_ADDR else 0; 
    idaddr <= CONV_INTEGER(daddr - DS_ADDR)/4 when daddr >= DS_ADDR else 0;

-- Instruction Memory setup    
-- Load code memory form file m_in.txt

    DO_MEM: process(RES, niR, caddr) is
         procedure LOAD_IMEM is --return mem_array is
           -- variable mem: mem_array;
            file my_file: text open read_mode is "m_in.txt";
            variable L: line;
            variable nAddr: natural;
            variable Addr: Word; 
            variable data: Word;
-----------            variable CL: line;
        begin
            --write(CL, string'("DATA = **********************************************"));
            naddr := 0;
            while not endfile(my_file) loop
                readline(my_file,L);
                hread(L, Data);
                IMEM(naddr) <= Data;
                naddr := naddr + 1;
            end loop;
        end LOAD_IMEM;   
    begin
        if RES'EVENT and RES = '1' then
            LOAD_IMEM; -- Load program in m_in.txt into code memory
        end if;
    end process;
-- Code memory read
    process(nIR, iaddr)
    begin
        if nIR = '0' then
            IM <= IMEM(iaddr);
        else
            IM <= (others => 'Z');
        end if;
    end process;
-- Data memory read and write    
    process(nWR, nRD, idaddr)
    begin        
        if nWR = '0' then
            Dmem(idaddr) <= DM;
        elsif nRD = '0' then
            DM <= Dmem(idaddr);
       else
            DM <= (others => 'Z');
        end if;
    end process;
----- Dump code memory --------------------------
    Final_Dump: process is
    -- Write memory to file m_out.txt     
        procedure DUMP_DMEM is
            file my_file: text open write_mode is "m_out.txt";
            variable L: line;
            variable str_out: string (1 to 8);
            variable i: natural := 0;
            variable addr, data: Word;
        begin
             for i in 0 to DMEM'length - 1 loop
                Addr := conv_std_logic_vector(i*4,32);
                hwrite (L, Addr);
                write (L, ' ');
                Data := DMEM(i);
                hwrite (L, Data); 
                writeline(my_file,L);
             end loop;
        end DUMP_DMEM;
    begin
       wait for 5 us;
       DUMP_DMEM;      -- Dump data memory
    end process;
------ End dump code memory -------------------------     
-- For debug output instruction fetched from code memory
    op <= IM(31 downto 26); 
    func <= IM(5 downto 0);
    process(nIR)
    begin
        if nIR'event and nIR = '1' then
            case op is
                when op_lw => inst <= LW;
                when op_sw => inst <= sw;
                when op_R_type =>
                    case FUNC IS
                        when Func_add => inst <= ADD;
                        when Func_or => inst <= iOR;
                        when Func_sub => inst <= SUB;
                        when Func_and => inst <= iAND;
                        when Func_slt => inst <= SLT;
                        when Func_jr => inst <= JR;
                        when Func_xor => inst <= iXOR;
                        when Func_nor => inst <= iNOR;
                        when others => inst <= BAD;
                    end case;    
                when op_J => inst <= J;
                when op_Jal => inst <= JAL;
                when op_lui => inst <= LUI;
                when op_andi => inst <= ANDI;
                when op_ori => inst <= ORI;
                when op_xori => inst <= XORI;
                when op_slti => inst <= SLTI;
                when op_beq => inst <= BEQ;
                when op_addi => inst <= ADDi;
                when others => inst <= BAD;
            end case;
        end if;
    end process;  
end architecture test;
