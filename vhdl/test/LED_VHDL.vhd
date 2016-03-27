----------------------------------------------------------------------------------
-- Company: Axelsys
-- Engineer: Greg Miller
-- 
-- Create Date:    10:08:13 04/30/2013 
-- Design Name: 	 LED LED
-- Module Name:    LED_VHDL 
-- Project Name:   iCE40 Evaluation Kit
-- Target Devices: iCE40-HX8K_CT256
-- Tool versions: 
-- Description: 
-- LEDs, D1 through D8 will blink on for 1/2 second and off for 1/2 second.
-- Clock is operating at 12MHz.
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.ALL;

entity LED_VHDL is
    Port ( clk : in  STD_LOGIC;
           LED1 : out  STD_LOGIC;
           LED2 : out  STD_LOGIC;
           LED3 : out  STD_LOGIC;
           LED4 : out  STD_LOGIC;
           LED5 : out  STD_LOGIC;
           LED6 : out  STD_LOGIC;
           LED7 : out  STD_LOGIC;
           LED8 : out  STD_LOGIC);
end LED_VHDL;

architecture Behavioral of LED_VHDL is

	signal div_cntr1 : std_logic_vector(15 downto 0) := (others => '0');
	signal div_cntr2 : std_logic_vector(6 downto 0)  := (others => '0');
	signal dec_cntr  : std_logic  := '0';
	signal half_sec_pulse : std_logic := '0';

begin
	
process(clk)
begin
	if rising_edge(clk) then
		div_cntr1 <= div_cntr1 + 1; 
		if div_cntr1 = 0 then
			if div_cntr2 = 91 then
				div_cntr2 <= (others => '0');
				half_sec_pulse <= '1';
			else
				div_cntr2 <= div_cntr2 + 1;
			end if;
		else
				half_sec_pulse <= '0';
		end if;
		
		if (half_sec_pulse = '1') then
			dec_cntr <= not dec_cntr;
		end if;
		
	end if;
end process;

	--LED decording
	LED1 <= '1' when (dec_cntr = '1') else '0';
	LED2 <= '1' when (dec_cntr = '1') else '0';
	LED3 <= '1' when (dec_cntr = '1') else '0';
	LED4 <= '1' when (dec_cntr = '1') else '0';
	LED5 <= '1' when (dec_cntr = '1') else '0';
	LED6 <= '1' when (dec_cntr = '1') else '0';
	LED7 <= '1' when (dec_cntr = '1') else '0';
	LED8 <= '1' when (dec_cntr = '1') else '0';
		
end Behavioral;

