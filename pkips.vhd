-------------------------------------------------------------------------------
--  pkips.vhd: Simple pipelined MIPS processor
--  Egre 426: Computer organization and design
--  Needs pkips in pkgpkips.vhd
-------------------------------------------------------------------------------
LiBRARY IEEE;
USE work.ALL;
USE IEEE.std_logic_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;
use pkg_pkips.all;

entity pkips is
  port(res:   in    std_logic;
       clk:   in    std_logic;
       caddr: out   std_logic_vector(31 downto 0);-- code memory addr
       daddr: out   std_logic_vector(31 downto 0);-- data memory addr
       im:    inout std_logic_vector(31 downto 0);-- instruction memory
       dm:    inout std_logic_vector(31 downto 0);-- data memory
       nir:   out   std_logic; -- not instruction read
       nrd:   out   std_logic; -- not read
       nwr:   out   std_logic);-- not write
  end;

architecture arch of pkips is
   -- Types, subtypes, and constants
   type regfiletype is array(0 to 31) of std_logic_vector(31 downto 0);
   type memtype is array(0 to memsize - 1) of std_logic_vector(31 downto 0);
   
   -- Global signals
   signal iRES : std_logic := '1';
   signal iNIR : std_logic := '0';
   signal ZERO : std_logic := '0';
   signal stall : std_logic := '0';
   
   -- Signals for the opcode that will get passed along the pipeline.
   signal IR_OP, IRX_OP, IRM_OP, IRW_OP: opcode := (others => 'X');
   
   -- The addresses of IR_Rs, IRX_Rs, IRM_Rs, and IRW_Rs.
   signal IR_Rs, IRX_Rs, IRM_Rs, IRW_Rs, checkWdest, checkMdest : integer range 0 to 31; 
   
   -- The addresses of IR_Rt, IRX_Rt, IRM_Rt, and IRW_Rt.
   signal IR_Rt, IRX_Rt, IRM_Rt, IRW_Rt: integer range 0 to 31;
   
   -- The addresses of IR_Rd, IRX_Rd, IRM_Rd, and IRW_Rd. 
   signal IR_Rd, IRX_Rd, IRM_Rd, IRW_Rd: integer range 0 to 31;

   -- Signals for shift amount, function code, address, and offset.
   signal IR_shamt, IRX_shamt, IRM_shamt, IRW_shamt : shamt := (others => 'X');
   signal IR_func, IRX_func, IRM_func, IRW_func : Func := (others => 'X');
   signal IR_addr, IRX_addr, IRM_addr, IRW_addr : Addr := (others => 'X');
   signal IR_n, IRX_n, IRM_n, IRW_n: nofst := (others => 'X');
   
   -- Signals for the Program Counter.
   signal PC:  STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- PC    
   signal PCX:  STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- PCX
    
   -- Signals for the instruction register. 
   signal IR:  STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- IR   
   signal IRX:  STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- IRX  
   signal IRM:  STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- IRM  
   signal IRW:  STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- IRW 
   
   -- Registers for A and B for where A and B assumes its values in the
   -- Instruction Decode stage and alpha and beta assumes its values in 
   -- the Execution stage.     
   signal A:   STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- A     
   signal B:   STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- B     
   signal alpha:   STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- alpha 
   signal beta:   STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- beta
   
   -- ALU result registers.  
   signal ALU: STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- ALUM 
   signal ALUM: STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- ALUM
   signal ALUW: STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- ALUW
   signal SMDR: STD_LOGIC_vector(31 downto 0) := (others => 'X'); -- SMDR
   signal GPR: regfiletype := (others => (others => 'X')); -- GPR 
   
   
   function extend32(n: in STD_LOGIC_vector(15 downto 0)) return STD_LOGIC_vector is 
      variable nextend: STD_LOGIC_vector(31 downto 0);
   begin
      nextend(15 downto 0) := n;
      nextend(31 downto 16) := (others => n(15));
      return nextend;
   end;
   
   
   function extend30(n: in nofst) return std_logic_vector is 
      variable nextend: std_logic_vector(29 downto 0);
   begin
      nextend(15 downto 0) := n;
      nextend(29 downto 16) := (others => n(15));
      return nextend;
   end;  
   
begin
   DM <= (others => 'Z');
   IM <= (others => 'Z');
   
   caddr <= PC when ires = '0' else (others => 'Z');
   
   -----------------------------------------------------------------------------
   --          Process: set_nIR
   -- Sensitivity list: clk 
   --      Description: Sets up the Instruction Read signal according 
   --                   to the events and values of the system clock.
   -----------------------------------------------------------------------------
   set_nIR : process(clk, ires)
   begin
      if clk'event and ires = '0' then
         if clk = '0' then
            nir <= '1';
            iNIR <= '1';
         elsif clk = '1' then
            nir <= '0';
            iNIR <= '0';
         else 
            nir <= '1';
            iNIR <= '1';
         end if;
      else 
         nir <= '1';
         iNIR <= '1';
      end if;
   end process;
   
   -----------------------------------------------------------------------------
   --          Process: set_Internal_RES
   -- Sensitivity list: 
   --      Description: Assign Internal Reset signal to change only on falling edge
   --                       of clock
   -----------------------------------------------------------------------------  
    
   set_Internal_RES : process(res, clk)
   begin
      if clk'event and clk = '0'and res = '1' then
         iRES <= '1';
      elsif clk'event and clk = '0'and res = '0' then
         iRES <= '0';
      end if;
   end process;
   
   
   ---  Do all ALU calculations
   
   AluOperation : process(IRX, alpha, beta)
   begin
      case IRX_OP is   
              -- If the instruction is an R-Type instruction we enter this 
              -- case statement.
         when op_R_Type =>
            case IRX_Func is
               when Func_add => 
                  ALU <= alpha + beta after 7 ns;
               when Func_or =>
                  ALU <= alpha or beta after 7 ns;
               when Func_sub =>
                  ALU <= alpha - beta after 7 ns;
               when Func_and =>
                  ALU <= alpha and beta after 7 ns;
               when Func_slt =>
                  if alpha < beta then
                     ALU <= x"00000001" after 1 ns;
                  elsif alpha >= beta then
                     ALU <= x"00000000" after 1 ns;
                  end if;
               when Func_xor =>
                  ALU <= alpha xor beta after 7 ns;
               when Func_nor =>
                  ALU <= not(alpha or beta) after 7 ns;
               when Func_nop =>                  -- Zero out ALU for no-op operation
                  ALU <= x"00000000" after 1 ns;               
               when others => null;
            end case;
            
            -- If the instruction is not an R-Type instruction we skip the 
            -- above case statement and we consider the following cases 
            -- to determine which instruction is going to be executed.
 
         when op_lui =>
            ALU <= IRX_n & x"0000" after 1 ns;
         when op_andi =>
            ALU <= alpha and extend32(IRX_n) after 7 ns; 
         when op_ori =>
            ALU <= alpha or x"0000" & IRX_n after 7 ns;
         when op_xori =>
            ALU <= alpha xor x"0000" & IRX_n after 7 ns;
         when op_slti => 
            if alpha < IRX_n then
               ALU <= x"00000001" after 7 ns;
            else 
               ALU <= x"00000000" after 7 ns;
            end if;
         when op_addi => 
            ALU <= alpha + extend32(IRX_n) after 7 ns;
         when op_lw =>
            ALU <= alpha + extend32(IRX_n) after 7 ns;
         when op_sw =>
            ALU <= alpha + extend32(IRX_n) after 7 ns;
         when others => null;
      end case;
   end process;
   
   
    -- Initiate data forwarding and stalls to handle data hazards.
    
   Forwarding : process(IRX, IRW, IRM, ALUM, ALUW, A, B, checkMdest, checkWdest)
   begin      
          -- checkWdest holds the register being changed in WB stage
          
      case IRW_op is
                   -- These  instructions change the Rd register
         when op_R_Type =>

            case IRW_func is
               when func_add | func_sub | func_and | func_or | func_slt 
                    | func_xor | func_nor =>
                  checkWdest <= IRW_Rd;   
               when others =>
                  checkWdest <= 0;
         end case;
     
                 -- These instructions change the Rt register
      when op_addi | op_andi  | op_lw | op_lui | op_ori | op_xori 
                      | op_slti =>                
         checkWdest <= IRW_Rt;
      when others =>           
         checkWdest <= 0;
      end case;
      
          -- checkMdest holds the register being changed in DM stage  
              
      case IRM_op is
         when op_R_Type =>
               -- These instructions change the Rd register
            case IRM_func is
               when func_add | func_sub | func_and | func_or | func_slt 
                    | func_xor | func_nor =>
                  checkMdest <= IRM_Rd;
               when others =>  
                   checkMdest <= 0;
            end case;
    
              -- These instructions change the Rt register
        when op_addi | op_andi  | op_lw | op_lui | op_ori | op_xori 
                     | op_slti =>    
           checkMdest <= IRM_Rt;                             
        when others =>     
           checkMdest <= 0;
             
      end case;
       
      -- Initiate a Stall if the previous op was a LW to a register needed in this instruction
       
      stall <= '0';
     
      if IRX_Rs = 0 
         then alpha <= A;
      elsif IRX_Rs = checkMdest then 
         if IRM_op = op_lw then 
            stall <= '1';
         else 
            alpha <= ALUM;
         end if;      
      elsif IRX_Rs = checkWdest then 
         alpha <= ALUW;
      else 
         alpha <= A;
      end if;
  


      if irX_Rt = 0 then 
         beta <= B;
      elsif IRX_Rt = checkMdest then 
         if IRM_op = op_lw then 
            stall <= '1';
         else 
            beta <= ALUM;
         end if; 
      elsif IRX_Rt = checkWdest then 
         beta <= ALUW;
      else 
         beta <= B;
      end if;    
         
          
   end process;
     
   
   -----------------------------------------------------------------------------
   --          Process: pipeline
   -- Sensitivity list: 
   --      Description:   The 5 pipeline stages
   ----------------------------------------------------------------------------- 
                  
   pipeline : process(ires, clk)
   begin

      -- We check to see if there has been a reset.  If a reset has occured then
      -- we set the program counter to the base address for the code memory.
      -- Otherwise we increment the PC by 4 and begin execution of instructions
      -- in the pipeline. 
       
      if ires'event and ires = '0' then
         PC <= cs_addr;
      
      -- *** Pipeline FALLING EDGE procedures
      elsif ires = '0' and clk'event and clk = '0' then    
      
     -----------------------------------------------------------------------------
     -- Entry point for the IF (Instruction fetch) stage.
     -----------------------------------------------------------------------------    
         
         -- If theres is a stall, do nothing in the ID and IF stages
         if stall = '0' then 
            PC <= PC + 4 after 1 ns; 
            IR <= IM after 1 ns;
         
         -- The following block of code represents the IF/ID pipeline register
         -- where these operations physically occur in a MIPS microprocessor. 
            IR_OP <= IM(31 downto 26) after 1 ns;
            IR_Rs <= CONV_INTEGER(IM(25 downto 21)) after 1 ns;
            IR_Rt <= CONV_INTEGER(IM(20 downto 16)) after 1 ns;
            IR_Rd <= CONV_INTEGER(IM(15 downto 11)) after 1 ns;
            IR_n <= IM(15 downto 0) after 1 ns;
            IR_Shamt <= IM(10 downto 6) after 1 ns;
            IR_Func <= IM(5 downto 0) after 1 ns;
            IR_addr <= IM(25 downto 0) after 1 ns;
      
         
   -----------------------------------------------------------------------------
   -- Entry point for the ID (Instruction decode and register file read) stage.
   -----------------------------------------------------------------------------  
      
            
            PCX <= PC after 1 ns;
             
            -- Assigns A with the appropriate opperand from the GPR
            if IR_Rs = 0 then
               A <= (others => '0');
            else
               A <= GPR(IR_Rs) after 1 ns;
            end if;
      
            -- Assigns B with the appropriate opperand from the GPR
            if IR_Rt = 0 then
               B <= (others => '0');
            else
               B <= GPR(IR_Rt) after 1 ns;
            end if;
         
         
            IRX <= IR after 1 ns;
         
            -- The following block of code represents the ID/EX pipeline register
            -- where these operations physically occur in a MIPS microprocessor. 
            IRX_OP <= IR_OP after 1 ns;
            IRX_Rs <= IR_Rs after 1 ns;
            IRX_Rt <= IR_Rt after 1 ns;
            IRX_Rd <= IR_Rd after 1 ns;
            IRX_n <= IR_n after 1 ns;
            IRX_Shamt <= IR_Shamt after 1 ns;
            IRX_Func <= IR_Func after 1 ns;
            IRX_addr <= IR_addr after 1 ns;
         
         
         
         end if;             ---   end of IF for STALL *******
       
         
   -----------------------------------------------------------------------------
   -- Entry point for the EX (Execute and address calculation) stage.
   -----------------------------------------------------------------------------
   

         -- If there is a stall, do nothing in the EX stage and move a NOP to 
         -- in the MEM stage
 
 
         if stall = '0' then   
            case IRX_OP is
               -- If the instruction is an R-Type instruction we enter this 
               -- case statement.
               when op_R_Type =>
                  case IRX_Func is
                     when Func_jr =>
                        PC <= alpha after 1 ns;
                     when others => 
                        ALUM <= ALU;
                  end case;
                   
                  -- If the instruction is not an R-Type instruction we skip the 
                  -- above case statement and we consider the following cases 
                  -- to determine which instruction is going to be executed.
               when op_j =>
                  PC <= PCX(31 downto 28) & IRX_addr & "00";
               when op_jal =>
                  PC <= alpha after 1 ns;
               when op_lui | op_andi | op_ori | op_xori | op_slti | op_addi
                           | op_lw  =>
                  ALUM <= ALU;
  
               when op_sw =>
                  ALUM <= ALU;
                  SMDR <= beta;
               when op_beq =>
                  if alpha = beta then
                     PC <= PCX + (extend30(IRX_n) & "00");
                     ZERO <= '1';
                  else
                     ZERO <= '0';
                  end if;
               when op_bne =>
                  if alpha /= beta then
                     PC <= PCX + (extend30(IRX_n) & "00");
                     ZERO <= '1';
                  else
                     ZERO <= '0';
                  end if;              
               when others => null;
            end case;
           
         
            -- Register transfer IRX to IRM
           
            IRM <= IRX after 1 ns;
            IRM_OP <= IRX_OP after 1 ns;
            IRM_Rs <= IRX_Rs after 1 ns;
            IRM_Rt <= IRX_Rt after 1 ns;
            IRM_Rd <= IRX_Rd after 1 ns;
            IRM_n <= IRX_n after 1 ns;
            IRM_Shamt <= IRX_Shamt after 1 ns;
            IRM_Func <= IRX_Func after 1 ns;
            IRM_addr <= IRX_addr after 1 ns;
         
         else
            -- Moving all zeroes (NOP) to the DM stage 
            IRM <= (others => '0');
            IRM_OP <= (others => '0');
            IRM_Rs <= 0;
            IRM_Rt <= 0;
            IRM_Rd <= 0;
            IRM_n <= (others => '0');
            IRM_Shamt <= (others => '0');
            IRM_Func <= (others => '0');
            IRM_addr <= (others => '0');
         end if;    
        
   -----------------------------------------------------------------------------
   -- Entry point for the MEM (Memory access) stage.
   -----------------------------------------------------------------------------
         -- This case statement is suppose to follow the description of the 
         -- MEM stage as noted in handout.
     
               
         case IRM_OP is
            when op_lw =>
               ALUW <= dm;
            when others =>
               -- when not op_lw we do ALUW <= ALUM
               ALUW <= ALUM after 1 ns;
         end case;
         
         -- We always do the following register transfer operation: IRW <= IRM
         
         IRW <= IRM after 1 ns; 
         IRW_OP <= IRM_OP after 1 ns;
         IRW_Rs <= IRM_Rs after 1 ns;
         IRW_Rt <= IRM_Rt after 1 ns;
         IRW_Rd <= IRM_Rd after 1 ns;
         IRW_n <= IRM_n after 1 ns;
         IRW_Shamt <= IRM_Shamt after 1 ns;
         IRW_Func <= IRM_Func after 1 ns;
         IRW_addr <= IRM_addr after 1 ns;
         
         
       -- ****** Pipeline RISING EDGE procedures     
      elsif ires = '0' and clk'event and clk = '1' then  
     
         
   -----------------------------------------------------------------------------
   -- Entry point for the WB (Write back) stage.
   -----------------------------------------------------------------------------
 
         case IRW_op is
            when op_R_Type =>
                
            -- This case statement assigns GPR(IRW_Rd)<= ALUW for 
            -- R-Type instructions.
               case IRW_func is
                  when func_add | func_sub | func_and | func_or | func_slt 
                                | func_xor | func_nor =>
                     if (IRW_Rd /= 0) then 
                        GPR(IRW_Rd) <= ALUW after 1 ns;
                     end if;
                  when others => null;
               end case;
     
            -- Assign immediate type results with the statement 
            -- GPR(IRW_Rt) <= ALUW
            when op_addi | op_andi  | op_lw | op_lui | op_ori | op_xori 
                         | op_slti =>
               if (IRW_Rt /= 0) then 
                  GPR(IRW_Rt) <= ALUW after 1 ns;
               end if; 
            when op_jal =>
               GPR(31) <= ALUW after 1 ns;        
            when others => null;
         end case;
  
      end if;    ---  End IF for the entire process
      
   end process;
   
   
  -----------------------------------------------------------------------------
  --          Process: set_nRD 
  -- Sensitivity list: 
  --      Description: Set the data memory read and write signals
  -----------------------------------------------------------------------------
   set_nRD : process(IRM, clk)
   begin
       
      if ires = '0' and clk'event and clk = '1' then
         case IRM_OP is
            when op_sw =>
               nrd <= '1';
               nwr <= '0';
               daddr <= ALUM;
               dm <= SMDR;
            when op_lw =>
               nrd <= '0';
               nwr <= '1';
               daddr <= ALUM;
               dm <= (others => 'Z');   
            when others =>
               nrd <= '1';
               nwr <= '1';
               daddr <= (others => 'Z');
               dm <= (others => 'Z');   
            end case;
        else
           nrd <= '1';
           nwr <= '1';
           daddr <= (others => 'Z');   
           dm <= (others => 'Z');   
        end if;
   end process;
end;
