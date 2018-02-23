LIBRARY IEEE;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

ENTITY seg_driver IS
   PORT(
      input : IN std_logic_vector(3 DOWNTO 0);
      oput : OUT std_logic_vector(0 TO 6)
   );
END seg_driver;

LIBRARY IEEE;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

ENTITY timer IS
	PORT(en : IN std_logic;
		  rst : IN std_logic;
	     t_clk : IN std_logic;
	     set_time : IN std_logic_vector(3 DOWNTO 0);
		  running : OUT std_logic;
		  HEX0_out : OUT std_logic_vector(0 TO 6);
        HEX1_out : OUT std_logic_vector(0 TO 6)
		  );
END timer;

LIBRARY IEEE;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;

ENTITY tlc IS
    PORT(clk : IN std_logic;
         request : IN std_logic;
         reset : IN std_logic;
		   key0 : IN std_logic;
         output : OUT std_logic_vector(4 DOWNTO 0);
		   disp_0 : OUT std_logic_vector(0 to 6);
		   disp_1 : OUT std_logic_vector(0 to 6)
        );
END tlc;


ARCHITECTURE seg_driver_arch of seg_driver IS
BEGIN
-- COMMON ANODE

--    0
--  - - -
--  |   |
-- 5|   |1
--  | 6 |
--  - - -
--  |   |
-- 4|   |2
--  |   |
--  - - -
--    3
   oput <=  "0000001" WHEN (input = "0000") ELSE  -- 0
            "1001111" WHEN (input = "0001") ELSE  -- 1
            "0010010" WHEN (input = "0010") ELSE  -- 2
            "0000110" WHEN (input = "0011") ELSE  -- 3
            "1001100" WHEN (input = "0100") ELSE  -- 4
            "0100100" WHEN (input = "0101") ELSE  -- 5
            "1100000" WHEN (input = "0110") ELSE  -- 6
            "0001111" WHEN (input = "0111") ELSE  -- 7
            "0000000" WHEN (input = "1000") ELSE  -- 8
            "0001100" WHEN (input = "1001") ELSE  -- 9
            "1111111";
END seg_driver_arch;

ARCHITECTURE timer_arch OF timer IS
-- PORT(en : IN std_logic;
--      rst : IN std_logic;
--      t_clk : IN std_logic;
--      set_time : IN std_logic_vector(3 DOWNTO 0);
--      running : OUT std_logic;
--      HEX0_out : OUT std_logic_vector(0 TO 6);
--      HEX1_out : OUT std_logic_vector(0 TO 6)
--     );
	SIGNAL set_units    : std_logic_vector(3 DOWNTO 0);
	SIGNAL set_tens     : std_logic_vector(3 DOWNTO 0);
	COMPONENT seg_driver
		PORT(
			input : IN std_logic_vector(3 DOWNTO 0);
			oput : OUT std_logic_vector(0 TO 6)
		);
	END COMPONENT;
BEGIN
		unit_digit:seg_driver
			PORT MAP(input => set_units,
			         oput => HEX0_out);
		tens_digit:seg_driver
			PORT MAP(input => set_tens,
			         oput => HEX1_out);

		PROCESS(t_clk)
		  VARIABLE count : INTEGER := 0;
		BEGIN
          IF falling_edge(t_clk) THEN
            IF rst = '1' THEN
              set_units <= "1111";  -- Blank displays
              set_tens <= "1111";
              running <= '0';  -- clear running flag
              count := (to_integer(unsigned(set_time)))*50000000;  -- Reset timer
            ELSIF en = '1' and count /= 0 THEN
              running <= '1';
              IF count > (9*50000000) THEN
                -- 10
                set_tens <= "0001";
                set_units <= "0000";
              ELSIF count > (8*50000000) THEN
                -- 9
                set_tens <= "0000";
                set_units <= "1001";
              ELSIF count > (7*50000000) THEN
                -- 8
                set_tens <= "0000";
                set_units <= "1000";
              ELSIF count > (6*50000000) THEN
                -- 7
                set_tens <= "0000";
                set_units <= "0111";
              ELSIF count > (5*50000000) THEN
                -- 6
                set_tens <= "0000";
                set_units <= "0110";
              ELSIF count > (4*50000000) THEN
                -- 5
                set_tens <= "0000";
                set_units <= "0101";
              ELSIF count > (3*50000000) THEN
                -- 4
                set_tens <= "0000";
                set_units <= "0100";
              ELSIF count > (2*50000000) THEN
                -- 3
                set_tens <= "0000";
                set_units <= "0011";
              ELSIF count > 50000000 THEN
                -- 2
                set_tens <= "0000";
                set_units <= "0010";
              ELSIF count > 0 THEN
                -- 1
                set_tens <= "0000";
                set_units <= "0001";
              END IF;
              count := count - 1;
            ELSIF en = '1' and count = 0 THEN
              -- 0, complete
              set_tens <= "0000";
              set_units <= "0000";
              running <= '0';
            END IF;
          END IF;
        END PROCESS;
END timer_arch;

ARCHITECTURE tlc_arch OF tlc IS
  -- Build an enumerated type for the state machine
  TYPE state_type IS (G, Y, R, G_blocking);

  -- Register to hold the current state
  SIGNAL state : state_type;
  SIGNAL flag : std_logic := '1';
  SIGNAL timer_rst : std_logic := '0';
  SIGNAL timer_set : std_logic_vector(3 DOWNTO 0);
  SIGNAL timer_running : std_logic;
  COMPONENT timer
  	PORT(en : IN std_logic;
  		  rst : IN std_logic;
  	     t_clk : IN std_logic;
  	     set_time : IN std_logic_vector(3 DOWNTO 0);
  		  running : OUT std_logic;
  		  HEX0_out : OUT std_logic_vector(0 TO 6);
        HEX1_out : OUT std_logic_vector(0 TO 6)
  		  );
  END COMPONENT;
BEGIN
  tim1:timer
    PORT MAP(en => key0,
             rst => timer_rst,
             t_clk => clk,
             set_time => timer_set,
             running => timer_running,
             HEX0_out => disp_0,
             HEX1_out => disp_1
            );
  -- Logic to advance to the next state
  PROCESS (clk)
  BEGIN
      IF rising_edge(clk) THEN
			IF reset = '0' THEN
			  state <= G;
			  timer_rst <= '1';
			  timer_set <= "0000";
			ELSE
			  IF timer_running = '1' THEN
				 IF state = G_blocking and request = '0' THEN
					flag <= '0';
				 END IF;
			  ELSE
				 IF timer_rst = '1' THEN
					timer_rst <= '0';  -- Now that the timer is programmed, start running
				 ELSE
					-- Timer stopped, now change state
					CASE state IS
						 WHEN G=>
							  IF request = '0' or flag = '0' THEN
								 flag <= '1';
								 state <= Y;
								 timer_rst <= '1';
								 timer_set <= "0101";
							  END IF;
						 WHEN Y=>
								 state <= R;
								 timer_rst <= '1';
								 timer_set <= "1010";
						 WHEN R=>
								 state <= G_blocking;
								 timer_rst <= '1';
								 timer_set <= "1010";
						 WHEN G_blocking =>
								  state <= G;
					END CASE;
				 END IF;
			  END IF;
			END IF;
		END IF;
  END PROCESS;
    -- Output depends solely on the current state
  PROCESS (state)
  BEGIN
    CASE state IS
      WHEN G =>
        output <= "10001";
      WHEN Y =>
        output <= "10010";
      WHEN R =>
        output <= "01100";
      WHEN G_blocking =>
  	     output <= "10001";
    END CASE;
  END PROCESS;
END tlc_arch;
