## This file is a general .xdc for the Nexys4 rev B board
## To use it in a project:
## - uncomment the lines corresponding to used pins
## - rename the used ports (in each line, after get_ports) according to the top level signal names in the project

# Clock signal
#Bank = 35, Pin name = IO_L12P_T1_MRCC_35,					Sch name = CLK100MHZ
set_property PACKAGE_PIN E3 [get_ports CLK_IN]
set_property IOSTANDARD LVCMOS33 [get_ports CLK_IN]
#	create_clock -add -name CLK_IN -period 10.00 -waveform {0 5} [get_ports CLK_IN]

# Switches
#Bank = 34, Pin name = IO_L21P_T3_DQS_34,					Sch name = SW0
set_property PACKAGE_PIN J15 [get_ports {SW[0]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[0]}]
#Bank = 34, Pin name = IO_25_34,							Sch name = SW1
set_property PACKAGE_PIN L16 [get_ports {SW[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[1]}]
#Bank = 34, Pin name = IO_L23P_T3_34,						Sch name = SW2
set_property PACKAGE_PIN M13 [get_ports {SW[2]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[2]}]
#Bank = 34, Pin name = IO_L19P_T3_34,						Sch name = SW3
set_property PACKAGE_PIN R15 [get_ports {SW[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[3]}]
#Bank = 34, Pin name = IO_L19N_T3_VREF_34,					Sch name = SW4
set_property PACKAGE_PIN R17 [get_ports {SW[4]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[4]}]
#Bank = 34, Pin name = IO_L20P_T3_34,						Sch name = SW5
set_property PACKAGE_PIN T18 [get_ports {SW[5]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[5]}]
#Bank = 34, Pin name = IO_L20N_T3_34,						Sch name = SW6
set_property PACKAGE_PIN U18 [get_ports {SW[6]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[6]}]
#Bank = 34, Pin name = IO_L10P_T1_34,						Sch name = SW7
set_property PACKAGE_PIN R13 [get_ports {SW[7]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[7]}]
#Bank = 34, Pin name = IO_L8P_T1-34,						Sch name = SW8
set_property PACKAGE_PIN T8 [get_ports {SW[8]}]
set_property IOSTANDARD LVCMOS18 [get_ports {SW[8]}]
#Bank = 34, Pin name = IO_L9N_T1_DQS_34,					Sch name = SW9
set_property PACKAGE_PIN U8 [get_ports {SW[9]}]
set_property IOSTANDARD LVCMOS18 [get_ports {SW[9]}]
#Bank = 34, Pin name = IO_L9P_T1_DQS_34,					Sch name = SW10
set_property PACKAGE_PIN R16 [get_ports {SW[10]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[10]}]
#Bank = 34, Pin name = IO_L11N_T1_MRCC_34,					Sch name = SW11
set_property PACKAGE_PIN T13 [get_ports {SW[11]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[11]}]
#Bank = 34, Pin name = IO_L17N_T2_34,						Sch name = SW12
set_property PACKAGE_PIN H6 [get_ports {SW[12]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[12]}]
#Bank = 34, Pin name = IO_L11P_T1_SRCC_34,					Sch name = SW13
set_property PACKAGE_PIN U12 [get_ports {SW[13]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[13]}]
#Bank = 34, Pin name = IO_L14N_T2_SRCC_34,					Sch name = SW14
set_property PACKAGE_PIN U11 [get_ports {SW[14]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[14]}]
#Bank = 34, Pin name = IO_L14P_T2_SRCC_34,					Sch name = SW15
set_property PACKAGE_PIN V10 [get_ports {SW[15]}]
set_property IOSTANDARD LVCMOS33 [get_ports {SW[15]}]



## LEDs
##Bank = 34, Pin name = IO_L24N_T3_34,						Sch name = LED0
#set_property PACKAGE_PIN T8 [get_ports {led[0]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[0]}]
##Bank = 34, Pin name = IO_L21N_T3_DQS_34,					Sch name = LED1
#set_property PACKAGE_PIN V9 [get_ports {led[1]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[1]}]
##Bank = 34, Pin name = IO_L24P_T3_34,						Sch name = LED2
#set_property PACKAGE_PIN R8 [get_ports {led[2]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[2]}]
##Bank = 34, Pin name = IO_L23N_T3_34,						Sch name = LED3
#set_property PACKAGE_PIN T6 [get_ports {led[3]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[3]}]
##Bank = 34, Pin name = IO_L12P_T1_MRCC_34,					Sch name = LED4
#set_property PACKAGE_PIN T5 [get_ports {led[4]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[4]}]
##Bank = 34, Pin name = IO_L12N_T1_MRCC_34,					Sch	name = LED5
#set_property PACKAGE_PIN T4 [get_ports {led[5]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[5]}]
##Bank = 34, Pin name = IO_L22P_T3_34,						Sch name = LED6
#set_property PACKAGE_PIN U7 [get_ports {led[6]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[6]}]
##Bank = 34, Pin name = IO_L22N_T3_34,						Sch name = LED7
#set_property PACKAGE_PIN U6 [get_ports {led[7]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[7]}]
##Bank = 34, Pin name = IO_L10N_T1_34,						Sch name = LED8
#set_property PACKAGE_PIN V4 [get_ports {led[8]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[8]}]
##Bank = 34, Pin name = IO_L8N_T1_34,						Sch name = LED9
#set_property PACKAGE_PIN U3 [get_ports {led[9]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[9]}]
##Bank = 34, Pin name = IO_L7N_T1_34,						Sch name = LED10
#set_property PACKAGE_PIN V1 [get_ports {led[10]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[10]}]
##Bank = 34, Pin name = IO_L17P_T2_34,						Sch name = LED11
#set_property PACKAGE_PIN R1 [get_ports {led[11]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[11]}]
##Bank = 34, Pin name = IO_L13N_T2_MRCC_34,					Sch name = LED12
#set_property PACKAGE_PIN P5 [get_ports {led[12]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[12]}]
##Bank = 34, Pin name = IO_L7P_T1_34,						Sch name = LED13
#set_property PACKAGE_PIN U1 [get_ports {led[13]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[13]}]
##Bank = 34, Pin name = IO_L15N_T2_DQS_34,					Sch name = LED14
#set_property PACKAGE_PIN R2 [get_ports {led[14]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[14]}]
##Bank = 34, Pin name = IO_L15P_T2_DQS_34,					Sch name = LED15
#set_property PACKAGE_PIN P2 [get_ports {led[15]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {led[15]}]

#Bank = 34, Pin name = IO_L5P_T0_34,						Sch name = LED16_R
set_property PACKAGE_PIN R12 [get_ports RGB1_Blue]
set_property IOSTANDARD LVCMOS33 [get_ports RGB1_Blue]
#Bank = 15, Pin name = IO_L5P_T0_AD9P_15,					Sch name = LED16_G
set_property PACKAGE_PIN M16 [get_ports RGB1_Green]
set_property IOSTANDARD LVCMOS33 [get_ports RGB1_Green]
#Bank = 35, Pin name = IO_L19N_T3_VREF_35,					Sch name = LED16_B
set_property PACKAGE_PIN N16 [get_ports RGB1_Red]
set_property IOSTANDARD LVCMOS33 [get_ports RGB1_Red]

##Bank = 34, Pin name = IO_0_34,								Sch name = LED17_R
#set_property PACKAGE_PIN K6 [get_ports RGB2_Red]
#set_property IOSTANDARD LVCMOS33 [get_ports RGB2_Red]
##Bank = 35, Pin name = IO_24P_T3_35,						Sch name =  LED17_G
#set_property PACKAGE_PIN H6 [get_ports RGB2_Green]
#set_property IOSTANDARD LVCMOS33 [get_ports RGB2_Green]
##Bank = CONFIG, Pin name = IO_L3N_T0_DQS_EMCCLK_14,			Sch name = LED17_B
#set_property PACKAGE_PIN L16 [get_ports RGB2_Blue]
#set_property IOSTANDARD LVCMOS33 [get_ports RGB2_Blue]



#7 segment display
#Bank = 34, Pin name = IO_L2N_T0_34,						Sch name = CA
set_property PACKAGE_PIN T10 [get_ports {sevenseg[0]}]
set_property IOSTANDARD LVCMOS33 [get_ports {sevenseg[0]}]
#Bank = 34, Pin name = IO_L3N_T0_DQS_34,					Sch name = CB
set_property PACKAGE_PIN R10 [get_ports {sevenseg[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {sevenseg[1]}]
#Bank = 34, Pin name = IO_L6N_T0_VREF_34,					Sch name = CC
set_property PACKAGE_PIN K16 [get_ports {sevenseg[2]}]
set_property IOSTANDARD LVCMOS33 [get_ports {sevenseg[2]}]
#Bank = 34, Pin name = IO_L5N_T0_34,						Sch name = CD
set_property PACKAGE_PIN K13 [get_ports {sevenseg[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {sevenseg[3]}]
#Bank = 34, Pin name = IO_L2P_T0_34,						Sch name = CE
set_property PACKAGE_PIN P15 [get_ports {sevenseg[4]}]
set_property IOSTANDARD LVCMOS33 [get_ports {sevenseg[4]}]
#Bank = 34, Pin name = IO_L4N_T0_34,						Sch name = CF
set_property PACKAGE_PIN T11 [get_ports {sevenseg[5]}]
set_property IOSTANDARD LVCMOS33 [get_ports {sevenseg[5]}]
#Bank = 34, Pin name = IO_L6P_T0_34,						Sch name = CG
set_property PACKAGE_PIN L18 [get_ports {sevenseg[6]}]
set_property IOSTANDARD LVCMOS33 [get_ports {sevenseg[6]}]

##Bank = 34, Pin name = IO_L16P_T2_34,						Sch name = DP
#set_property PACKAGE_PIN M4 [get_ports dp]
#set_property IOSTANDARD LVCMOS33 [get_ports dp]

#Bank = 34, Pin name = IO_L18N_T2_34,						Sch name = AN0
set_property PACKAGE_PIN J17 [get_ports {anode[0]}]
set_property IOSTANDARD LVCMOS33 [get_ports {anode[0]}]
#Bank = 34, Pin name = IO_L18P_T2_34,						Sch name = AN1
set_property PACKAGE_PIN J18 [get_ports {anode[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {anode[1]}]
#Bank = 34, Pin name = IO_L4P_T0_34,						Sch name = AN2
set_property PACKAGE_PIN T9 [get_ports {anode[2]}]
set_property IOSTANDARD LVCMOS33 [get_ports {anode[2]}]
#Bank = 34, Pin name = IO_L13_T2_MRCC_34,					Sch name = AN3
set_property PACKAGE_PIN J14 [get_ports {anode[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {anode[3]}]
#Bank = 34, Pin name = IO_L3P_T0_DQS_34,					Sch name = AN4
set_property PACKAGE_PIN P14 [get_ports {anode[4]}]
set_property IOSTANDARD LVCMOS33 [get_ports {anode[4]}]
#Bank = 34, Pin name = IO_L16N_T2_34,						Sch name = AN5
set_property PACKAGE_PIN T14 [get_ports {anode[5]}]
set_property IOSTANDARD LVCMOS33 [get_ports {anode[5]}]
#Bank = 34, Pin name = IO_L1P_T0_34,						Sch name = AN6
set_property PACKAGE_PIN K2 [get_ports {anode[6]}]
set_property IOSTANDARD LVCMOS33 [get_ports {anode[6]}]
#Bank = 34, Pin name = IO_L1N_T034,							Sch name = AN7
set_property PACKAGE_PIN U13 [get_ports {anode[7]}]
set_property IOSTANDARD LVCMOS33 [get_ports {anode[7]}]



#Buttons
#Bank = 15, Pin name = IO_L3P_T0_DQS_AD1P_15,				Sch name = CPU_RESET
set_property PACKAGE_PIN C12 [get_ports CPUreset]
set_property IOSTANDARD LVCMOS33 [get_ports CPUreset]
##Bank = 15, Pin name = IO_L11N_T1_SRCC_15,					Sch name = BTNC
#set_property PACKAGE_PIN E16 [get_ports btnC]
#set_property IOSTANDARD LVCMOS33 [get_ports btnC]
##Bank = 15, Pin name = IO_L14P_T2_SRCC_15,					Sch name = BTNU
#set_property PACKAGE_PIN F15 [get_ports btnU]
#set_property IOSTANDARD LVCMOS33 [get_ports btnU]
##Bank = CONFIG, Pin name = IO_L15N_T2_DQS_DOUT_CSO_B_14,	Sch name = BTNL
#set_property PACKAGE_PIN T16 [get_ports btnL]
#set_property IOSTANDARD LVCMOS33 [get_ports btnL]
##Bank = 14, Pin name = IO_25_14,							Sch name = BTNR
#set_property PACKAGE_PIN R10 [get_ports btnR]
#set_property IOSTANDARD LVCMOS33 [get_ports btnR]
##Bank = 14, Pin name = IO_L21P_T3_DQS_14,					Sch name = BTND
#set_property PACKAGE_PIN V10 [get_ports btnD]
#set_property IOSTANDARD LVCMOS33 [get_ports btnD]



##Pmod Header JA
##Bank = 15, Pin name = IO_L1N_T0_AD0N_15,					Sch name = JA1
#set_property PACKAGE_PIN B13 [get_ports {JA[0]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JA[0]}]
##Bank = 15, Pin name = IO_L5N_T0_AD9N_15,					Sch name = JA2
#set_property PACKAGE_PIN F14 [get_ports {JA[1]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JA[1]}]
##Bank = 15, Pin name = IO_L16N_T2_A27_15,					Sch name = JA3
#set_property PACKAGE_PIN D17 [get_ports {JA[2]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JA[2]}]
##Bank = 15, Pin name = IO_L16P_T2_A28_15,					Sch name = JA4
#set_property PACKAGE_PIN E17 [get_ports {JA[3]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JA[3]}]
##Bank = 15, Pin name = IO_0_15,								Sch name = JA7
#set_property PACKAGE_PIN G13 [get_ports {JA[4]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JA[4]}]
##Bank = 15, Pin name = IO_L20N_T3_A19_15,					Sch name = JA8
#set_property PACKAGE_PIN C17 [get_ports {JA[5]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JA[5]}]
##Bank = 15, Pin name = IO_L21N_T3_A17_15,					Sch name = JA9
#set_property PACKAGE_PIN D18 [get_ports {JA[6]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JA[6]}]
##Bank = 15, Pin name = IO_L21P_T3_DQS_15,					Sch name = JA10
#set_property PACKAGE_PIN E18 [get_ports {JA[7]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JA[7]}]



#Pmod Header JB
#Bank = 15, Pin name = IO_L15N_T2_DQS_ADV_B_15,				Sch name = JB1
set_property PACKAGE_PIN D14 [get_ports {JB[0]}]
set_property IOSTANDARD LVCMOS33 [get_ports {JB[0]}]
#Bank = 14, Pin name = IO_L13P_T2_MRCC_14,					Sch name = JB2
set_property PACKAGE_PIN F16 [get_ports {JB[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {JB[1]}]
#Bank = 14, Pin name = IO_L21N_T3_DQS_A06_D22_14,			Sch name = JB3
set_property PACKAGE_PIN G16 [get_ports {JB[2]}]
set_property IOSTANDARD LVCMOS33 [get_ports {JB[2]}]
#Bank = CONFIG, Pin name = IO_L16P_T2_CSI_B_14,				Sch name = JB4
set_property PACKAGE_PIN H14 [get_ports {JB[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {JB[3]}]
#Bank = 15, Pin name = IO_25_15,							Sch name = JB7
set_property PACKAGE_PIN E16 [get_ports {JB[4]}]
set_property IOSTANDARD LVCMOS33 [get_ports {JB[4]}]
#Bank = CONFIG, Pin name = IO_L15P_T2_DQS_RWR_B_14,			Sch name = JB8
set_property PACKAGE_PIN F13 [get_ports {JB[5]}]
set_property IOSTANDARD LVCMOS33 [get_ports {JB[5]}]
#Bank = 14, Pin name = IO_L24P_T3_A01_D17_14,				Sch name = JB9
set_property PACKAGE_PIN G13 [get_ports {JB[6]}]
set_property IOSTANDARD LVCMOS33 [get_ports {JB[6]}]
#Bank = 14, Pin name = IO_L19N_T3_A09_D25_VREF_14,			Sch name = JB10
set_property PACKAGE_PIN H16 [get_ports {JB[7]}]
set_property IOSTANDARD LVCMOS33 [get_ports {JB[7]}]



##Pmod Header JC
##Bank = 35, Pin name = IO_L23P_T3_35,						Sch name = JC1
#set_property PACKAGE_PIN K2 [get_ports {JC[0]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JC[0]}]
##Bank = 35, Pin name = IO_L6P_T0_35,						Sch name = JC2
#set_property PACKAGE_PIN E7 [get_ports {JC[1]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JC[1]}]
##Bank = 35, Pin name = IO_L22P_T3_35,						Sch name = JC3
#set_property PACKAGE_PIN J3 [get_ports {JC[2]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JC[2]}]
##Bank = 35, Pin name = IO_L21P_T3_DQS_35,					Sch name = JC4
#set_property PACKAGE_PIN J4 [get_ports {JC[3]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JC[3]}]
##Bank = 35, Pin name = IO_L23N_T3_35,						Sch name = JC7
#set_property PACKAGE_PIN K1 [get_ports {JC[4]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JC[4]}]
##Bank = 35, Pin name = IO_L5P_T0_AD13P_35,					Sch name = JC8
#set_property PACKAGE_PIN E6 [get_ports {JC[5]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JC[5]}]
#Bank = 35, Pin name = IO_L22N_T3_35,						Sch name = JC9
set_property PACKAGE_PIN J4 [get_ports RXD_S0]
set_property IOSTANDARD LVCMOS33 [get_ports RXD_S0]
#Bank = 35, Pin name = IO_L19P_T3_35,						Sch name = JC10
set_property PACKAGE_PIN E6 [get_ports TXD_S0]
set_property IOSTANDARD LVCMOS33 [get_ports TXD_S0]



##Pmod Header JD
##Bank = 35, Pin name = IO_L21N_T2_DQS_35,					Sch name = JD1
#set_property PACKAGE_PIN H4 [get_ports {JD[0]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JD[0]}]
##Bank = 35, Pin name = IO_L17P_T2_35,						Sch name = JD2
#set_property PACKAGE_PIN H1 [get_ports {JD[1]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JD[1]}]
##Bank = 35, Pin name = IO_L17N_T2_35,						Sch name = JD3
#set_property PACKAGE_PIN G1 [get_ports {JD[2]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JD[2]}]
##Bank = 35, Pin name = IO_L20N_T3_35,						Sch name = JD4
#set_property PACKAGE_PIN G3 [get_ports {JD[3]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JD[3]}]
##Bank = 35, Pin name = IO_L15P_T2_DQS_35,					Sch name = JD7
#set_property PACKAGE_PIN H2 [get_ports {JD[4]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JD[4]}]
##Bank = 35, Pin name = IO_L20P_T3_35,						Sch name = JD8
#set_property PACKAGE_PIN G4 [get_ports {JD[5]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JD[5]}]
##Bank = 35, Pin name = IO_L15N_T2_DQS_35,					Sch name = JD9
#set_property PACKAGE_PIN G2 [get_ports {JD[6]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JD[6]}]
##Bank = 35, Pin name = IO_L13N_T2_MRCC_35,					Sch name = JD10
#set_property PACKAGE_PIN F3 [get_ports {JD[7]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JD[7]}]



##Pmod Header JXADC
##Bank = 15, Pin name = IO_L9P_T1_DQS_AD3P_15,				Sch name = XADC1_P -> XA1_P
#set_property PACKAGE_PIN A13 [get_ports {JXADC[0]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JXADC[0]}]
##Bank = 15, Pin name = IO_L8P_T1_AD10P_15,					Sch name = XADC2_P -> XA2_P
#set_property PACKAGE_PIN A15 [get_ports {JXADC[1]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JXADC[1]}]
##Bank = 15, Pin name = IO_L7P_T1_AD2P_15,					Sch name = XADC3_P -> XA3_P
#set_property PACKAGE_PIN B16 [get_ports {JXADC[2]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JXADC[2]}]
##Bank = 15, Pin name = IO_L10P_T1_AD11P_15,					Sch name = XADC4_P -> XA4_P
#set_property PACKAGE_PIN B18 [get_ports {JXADC[3]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JXADC[3]}]
##Bank = 15, Pin name = IO_L9N_T1_DQS_AD3N_15,				Sch name = XADC1_N -> XA1_N
#set_property PACKAGE_PIN A14 [get_ports {JXADC[4]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JXADC[4]}]
##Bank = 15, Pin name = IO_L8N_T1_AD10N_15,					Sch name = XADC2_N -> XA2_N
#set_property PACKAGE_PIN A16 [get_ports {JXADC[5]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JXADC[5]}]
##Bank = 15, Pin name = IO_L7N_T1_AD2N_15,					Sch name = XADC3_N -> XA3_N
#set_property PACKAGE_PIN B17 [get_ports {JXADC[6]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JXADC[6]}]
##Bank = 15, Pin name = IO_L10N_T1_AD11N_15,					Sch name = XADC4_N -> XA4_N
#set_property PACKAGE_PIN A18 [get_ports {JXADC[7]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {JXADC[7]}]



#VGA Connector
#Bank = 35, Pin name = IO_L8N_T1_AD14N_35,					Sch name = VGA_R0
set_property PACKAGE_PIN B7 [get_ports {RGB[0]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[0]}]
#Bank = 35, Pin name = IO_L7N_T1_AD6N_35,					Sch name = VGA_R1
set_property PACKAGE_PIN C7 [get_ports {RGB[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[1]}]
#Bank = 35, Pin name = IO_L1N_T0_AD4N_35,					Sch name = VGA_R2
set_property PACKAGE_PIN D7 [get_ports {RGB[2]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[2]}]
#Bank = 35, Pin name = IO_L8P_T1_AD14P_35,					Sch name = VGA_R3
set_property PACKAGE_PIN D8 [get_ports {RGB[3]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[3]}]
#Bank = 35, Pin name = IO_L2P_T0_AD12P_35,					Sch name = VGA_B0
set_property PACKAGE_PIN A3 [get_ports {RGB[8]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[8]}]
#Bank = 35, Pin name = IO_L4N_T0_35,						Sch name = VGA_B1
set_property PACKAGE_PIN B4 [get_ports {RGB[9]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[9]}]
#Bank = 35, Pin name = IO_L6N_T0_VREF_35,					Sch name = VGA_B2
set_property PACKAGE_PIN C5 [get_ports {RGB[10]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[10]}]
#Bank = 35, Pin name = IO_L4P_T0_35,						Sch name = VGA_B3
set_property PACKAGE_PIN A4 [get_ports {RGB[11]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[11]}]
#Bank = 35, Pin name = IO_L1P_T0_AD4P_35,					Sch name = VGA_G0
set_property PACKAGE_PIN C6 [get_ports {RGB[4]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[4]}]
#Bank = 35, Pin name = IO_L3N_T0_DQS_AD5N_35,				Sch name = VGA_G1
set_property PACKAGE_PIN A5 [get_ports {RGB[5]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[5]}]
#Bank = 35, Pin name = IO_L2N_T0_AD12N_35,					Sch name = VGA_G2
set_property PACKAGE_PIN B6 [get_ports {RGB[6]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[6]}]
#Bank = 35, Pin name = IO_L3P_T0_DQS_AD5P_35,				Sch name = VGA_G3
set_property PACKAGE_PIN A6 [get_ports {RGB[7]}]
set_property IOSTANDARD LVCMOS33 [get_ports {RGB[7]}]
#Bank = 15, Pin name = IO_L4P_T0_15,						Sch name = VGA_HS
set_property PACKAGE_PIN B11 [get_ports HSync]
set_property IOSTANDARD LVCMOS33 [get_ports HSync]
#Bank = 15, Pin name = IO_L3N_T0_DQS_AD1N_15,				Sch name = VGA_VS
set_property PACKAGE_PIN B12 [get_ports VSync]
set_property IOSTANDARD LVCMOS33 [get_ports VSync]



#Micro SD Connector
#Bank = 35, Pin name = IO_L14P_T2_SRCC_35,					Sch name = SD_RESET
set_property PACKAGE_PIN E2 [get_ports SD_RESET]
set_property IOSTANDARD LVCMOS33 [get_ports SD_RESET]
#Bank = 35, Pin name = IO_L9N_T1_DQS_AD7N_35,				Sch name = SD_CD
set_property PACKAGE_PIN A1 [get_ports SD_CD]
set_property IOSTANDARD LVCMOS33 [get_ports SD_CD]
#Bank = 35, Pin name = IO_L9P_T1_DQS_AD7P_35,				Sch name = SD_SCK
set_property PACKAGE_PIN B1 [get_ports SCK]
set_property IOSTANDARD LVCMOS33 [get_ports SCK]
#Bank = 35, Pin name = IO_L16N_T2_35,						Sch name = SD_CMD
set_property PACKAGE_PIN C1 [get_ports MOSI]
set_property IOSTANDARD LVCMOS33 [get_ports MOSI]
#Bank = 35, Pin name = IO_L16P_T2_35,						Sch name = SD_DAT0
set_property PACKAGE_PIN C2 [get_ports MISO]
set_property IOSTANDARD LVCMOS33 [get_ports MISO]
##Bank = 35, Pin name = IO_L18N_T2_35,						Sch name = SD_DAT1
#set_property PACKAGE_PIN E1 [get_ports {sdData[1]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {sdData[1]}]
#Bank = 35, Pin name = IO_L18P_T2_35,						Sch name = SD_DAT2
#set_property PACKAGE_PIN F1 [get_ports {sdData[2]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {sdData[2]}]
#Bank = 35, Pin name = IO_L14N_T2_SRCC_35,					Sch name = SD_DAT3
set_property PACKAGE_PIN D2 [get_ports SD_CS]
set_property IOSTANDARD LVCMOS33 [get_ports SD_CS]



##Accelerometer
##Bank = 15, Pin name = IO_L6N_T0_VREF_15,					Sch name = ACL_MISO
#set_property PACKAGE_PIN D13 [get_ports aclMISO]
#set_property IOSTANDARD LVCMOS33 [get_ports aclMISO]
##Bank = 15, Pin name = IO_L2N_T0_AD8N_15,					Sch name = ACL_MOSI
#set_property PACKAGE_PIN B14 [get_ports aclMOSI]
#set_property IOSTANDARD LVCMOS33 [get_ports aclMOSI]
##Bank = 15, Pin name = IO_L12P_T1_MRCC_15,					Sch name = ACL_SCLK
#set_property PACKAGE_PIN D15 [get_ports aclSCK]
#set_property IOSTANDARD LVCMOS33 [get_ports aclSCK]
##Bank = 15, Pin name = IO_L12N_T1_MRCC_15,					Sch name = ACL_CSN
#set_property PACKAGE_PIN C15 [get_ports aclSS]
#set_property IOSTANDARD LVCMOS33 [get_ports aclSS]
##Bank = 15, Pin name = IO_L20P_T3_A20_15,					Sch name = ACL_INT1
#set_property PACKAGE_PIN C16 [get_ports aclInt1]
#set_property IOSTANDARD LVCMOS33 [get_ports aclInt1]
##Bank = 15, Pin name = IO_L11P_T1_SRCC_15,					Sch name = ACL_INT2
#set_property PACKAGE_PIN E15 [get_ports aclInt2]
#set_property IOSTANDARD LVCMOS33 [get_ports aclInt2]



##Temperature Sensor
##Bank = 15, Pin name = IO_L14N_T2_SRCC_15,					Sch name = TMP_SCL
#set_property PACKAGE_PIN F16 [get_ports tmpSCL]
#set_property IOSTANDARD LVCMOS33 [get_ports tmpSCL]
##Bank = 15, Pin name = IO_L13N_T2_MRCC_15,					Sch name = TMP_SDA
#set_property PACKAGE_PIN G16 [get_ports tmpSDA]
#set_property IOSTANDARD LVCMOS33 [get_ports tmpSDA]
##Bank = 15, Pin name = IO_L1P_T0_AD0P_15,					Sch name = TMP_INT
#set_property PACKAGE_PIN D14 [get_ports tmpInt]
#set_property IOSTANDARD LVCMOS33 [get_ports tmpInt]
##Bank = 15, Pin name = IO_L1N_T0_AD0N_15,					Sch name = TMP_CT
#set_property PACKAGE_PIN C14 [get_ports tmpCT]
#set_property IOSTANDARD LVCMOS33 [get_ports tmpCT]



##Omnidirectional Microphone
##Bank = 35, Pin name = IO_25_35,							Sch name = M_CLK
#set_property PACKAGE_PIN J5 [get_ports micClk]
#set_property IOSTANDARD LVCMOS33 [get_ports micClk]
##Bank = 35, Pin name = IO_L24N_T3_35,						Sch name = M_DATA
#set_property PACKAGE_PIN H5 [get_ports micData]
#set_property IOSTANDARD LVCMOS33 [get_ports micData]
##Bank = 35, Pin name = IO_0_35,								Sch name = M_LRSEL
#set_property PACKAGE_PIN F5 [get_ports micLRSel]
#set_property IOSTANDARD LVCMOS33 [get_ports micLRSel]



##PWM Audio Amplifier
##Bank = 15, Pin name = IO_L4N_T0_15,						Sch name = AUD_PWM
#set_property PACKAGE_PIN A11 [get_ports ampPWM]
#set_property IOSTANDARD LVCMOS33 [get_ports ampPWM]
##Bank = 15, Pin name = IO_L6P_T0_15,						Sch name = AUD_SD
#set_property PACKAGE_PIN D12 [get_ports ampSD]
#set_property IOSTANDARD LVCMOS33 [get_ports ampSD]


##USB-RS232 Interface
##Bank = 35, Pin name = IO_L7P_T1_AD6P_35,					Sch name = UART_TXD_IN
#set_property PACKAGE_PIN C4 [get_ports RsRx]
#set_property IOSTANDARD LVCMOS33 [get_ports RsRx]
##Bank = 35, Pin name = IO_L11N_T1_SRCC_35,					Sch name = UART_RXD_OUT
#set_property PACKAGE_PIN D4 [get_ports RsTx]
#set_property IOSTANDARD LVCMOS33 [get_ports RsTx]
##Bank = 35, Pin name = IO_L12N_T1_MRCC_35,					Sch name = UART_CTS
#set_property PACKAGE_PIN D3 [get_ports RsCts]
#set_property IOSTANDARD LVCMOS33 [get_ports RsCts]
##Bank = 35, Pin name = IO_L5N_T0_AD13N_35,					Sch name = UART_RTS
#set_property PACKAGE_PIN E5 [get_ports RsRts]
#set_property IOSTANDARD LVCMOS33 [get_ports RsRts]



#USB HID (PS/2)
#Bank = 35, Pin name = IO_L13P_T2_MRCC_35,					Sch name = PS2_CLK
set_property PACKAGE_PIN F4 [get_ports PS2C]
set_property IOSTANDARD LVCMOS33 [get_ports PS2C]
set_property PULLUP true [get_ports PS2C]
#Bank = 35, Pin name = IO_L10N_T1_AD15N_35,					Sch name = PS2_DATA
set_property PACKAGE_PIN B2 [get_ports PS2D]
set_property IOSTANDARD LVCMOS33 [get_ports PS2D]
set_property PULLUP true [get_ports PS2D]



#SMSC Ethernet PHY
#Bank = 16, Pin name = IO_L11P_T1_SRCC_16,					Sch name = ETH_MDC
set_property PACKAGE_PIN C9 [get_ports PHYMDC]
set_property IOSTANDARD LVCMOS33 [get_ports PHYMDC]
#Bank = 16, Pin name = IO_L14N_T2_SRCC_16,					Sch name = ETH_MDIO
set_property PACKAGE_PIN A9 [get_ports PHYMDIO]
set_property IOSTANDARD LVCMOS33 [get_ports PHYMDIO]
#Bank = 35, Pin name = IO_L10P_T1_AD15P_35,					Sch name = ETH_RSTN
set_property PACKAGE_PIN B3 [get_ports PHYRSTN]
set_property IOSTANDARD LVCMOS33 [get_ports PHYRSTN]
#Bank = 16, Pin name = IO_L6N_T0_VREF_16,					Sch name = ETH_CRSDV
set_property PACKAGE_PIN D9 [get_ports PHYCRS]
set_property IOSTANDARD LVCMOS33 [get_ports PHYCRS]
#Bank = 16, Pin name = IO_L13N_T2_MRCC_16,					Sch name = ETH_RXERR
set_property PACKAGE_PIN C10 [get_ports PHYRXERR]
set_property IOSTANDARD LVCMOS33 [get_ports PHYRXERR]
#Bank = 16, Pin name = IO_L19N_T3_VREF_16,					Sch name = ETH_RXD0
set_property PACKAGE_PIN C11 [get_ports {PHYRXD[0]}]
set_property IOSTANDARD LVCMOS33 [get_ports {PHYRXD[0]}]
#Bank = 16, Pin name = IO_L13P_T2_MRCC_16,					Sch name = ETH_RXD1
set_property PACKAGE_PIN D10 [get_ports {PHYRXD[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {PHYRXD[1]}]
#Bank = 16, Pin name = IO_L11N_T1_SRCC_16,					Sch name = ETH_TXEN
set_property PACKAGE_PIN B9 [get_ports PHYTXEN]
set_property IOSTANDARD LVCMOS33 [get_ports PHYTXEN]
#Bank = 16, Pin name = IO_L14P_T2_SRCC_16,					Sch name = ETH_TXD0
set_property PACKAGE_PIN A10 [get_ports {PHYTXD[0]}]
set_property IOSTANDARD LVCMOS33 [get_ports {PHYTXD[0]}]
#Bank = 16, Pin name = IO_L12N_T1_MRCC_16,					Sch name = ETH_TXD1
set_property PACKAGE_PIN A8 [get_ports {PHYTXD[1]}]
set_property IOSTANDARD LVCMOS33 [get_ports {PHYTXD[1]}]
#Bank = 35, Pin name = IO_L11P_T1_SRCC_35,					Sch name = ETH_REFCLK
set_property PACKAGE_PIN D5 [get_ports PHYCLK50MHZ]
set_property IOSTANDARD LVCMOS33 [get_ports PHYCLK50MHZ]
#Bank = 16, Pin name = IO_L12P_T1_MRCC_16,					Sch name = ETH_INTN
set_property PACKAGE_PIN B8 [get_ports PHYINTN]
set_property IOSTANDARD LVCMOS33 [get_ports PHYINTN]



##Quad SPI Flash
##Bank = CONFIG, Pin name = CCLK_0,							Sch name = QSPI_SCK
#set_property PACKAGE_PIN E9 [get_ports {QspiSCK}]
#set_property IOSTANDARD LVCMOS33 [get_ports {QspiSCK}]
##Bank = CONFIG, Pin name = IO_L1P_T0_D00_MOSI_14,			Sch name = QSPI_DQ0
#set_property PACKAGE_PIN K17 [get_ports {QspiDB[0]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {QspiDB[0]}]
##Bank = CONFIG, Pin name = IO_L1N_T0_D01_DIN_14,			Sch name = QSPI_DQ1
#set_property PACKAGE_PIN K18 [get_ports {QspiDB[1]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {QspiDB[1]}]
##Bank = CONFIG, Pin name = IO_L20_T0_D02_14,				Sch name = QSPI_DQ2
#set_property PACKAGE_PIN L14 [get_ports {QspiDB[2]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {QspiDB[2]}]
##Bank = CONFIG, Pin name = IO_L2P_T0_D03_14,				Sch name = QSPI_DQ3
#set_property PACKAGE_PIN M14 [get_ports {QspiDB[3]}]
#set_property IOSTANDARD LVCMOS33 [get_ports {QspiDB[3]}]
##Bank = CONFIG, Pin name = IO_L15N_T2_DQS_DOUT_CSO_B_14,	Sch name = QSPI_CSN
#set_property PACKAGE_PIN L13 [get_ports QspiCSn]
#set_property IOSTANDARD LVCMOS33 [get_ports QspiCSn]


##Bank = 14, Pin name = IO_L5P_T0_DQ06_14,					Sch name = CRAM_DQ0
#set_property PACKAGE_PIN R12 [get_ports {DATA_SDRAM[0]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[0]}]
##Bank = 14, Pin name = IO_L19P_T3_A10_D26_14,				Sch name = CRAM_DQ1
#set_property PACKAGE_PIN T11 [get_ports {DATA_SDRAM[1]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[1]}]
##Bank = 14, Pin name = IO_L20P_T3_A08)D24_14,				Sch name = CRAM_DQ2
#set_property PACKAGE_PIN U12 [get_ports {DATA_SDRAM[2]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[2]}]
##Bank = 14, Pin name = IO_L5N_T0_D07_14,					Sch name = CRAM_DQ3
#set_property PACKAGE_PIN R13 [get_ports {DATA_SDRAM[3]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[3]}]
##Bank = 14, Pin name = IO_L17N_T2_A13_D29_14,				Sch name = CRAM_DQ4
#set_property PACKAGE_PIN U18 [get_ports {DATA_SDRAM[4]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[4]}]
##Bank = 14, Pin name = IO_L12N_T1_MRCC_14,					Sch name = CRAM_DQ5
#set_property PACKAGE_PIN R17 [get_ports {DATA_SDRAM[5]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[5]}]
##Bank = 14, Pin name = IO_L7N_T1_D10_14,					Sch name = CRAM_DQ6
#set_property PACKAGE_PIN T18 [get_ports {DATA_SDRAM[6]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[6]}]
##Bank = 14, Pin name = IO_L7P_T1_D09_14,					Sch name = CRAM_DQ7
#set_property PACKAGE_PIN R18 [get_ports {DATA_SDRAM[7]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[7]}]
##Bank = 15, Pin name = IO_L22N_T3_A16_15,					Sch name = CRAM_DQ8
#set_property PACKAGE_PIN F18 [get_ports {DATA_SDRAM[8]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[8]}]
##Bank = 15, Pin name = IO_L22P_T3_A17_15,					Sch name = CRAM_DQ9
#set_property PACKAGE_PIN G18 [get_ports {DATA_SDRAM[9]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[9]}]
##Bank = 15, Pin name = IO_IO_L18N_T2_A23_15,				Sch name = CRAM_DQ10
#set_property PACKAGE_PIN G17 [get_ports {DATA_SDRAM[10]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[10]}]
##Bank = 14, Pin name = IO_L4N_T0_D05_14,					Sch name = CRAM_DQ11
#set_property PACKAGE_PIN M18 [get_ports {DATA_SDRAM[11]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[11]}]
##Bank = 14, Pin name = IO_L10N_T1_D15_14,					Sch name = CRAM_DQ12
#set_property PACKAGE_PIN M17 [get_ports {DATA_SDRAM[12]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[12]}]
##Bank = 14, Pin name = IO_L9N_T1_DQS_D13_14,				Sch name = CRAM_DQ13
#set_property PACKAGE_PIN P18 [get_ports {DATA_SDRAM[13]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[13]}]
##Bank = 14, Pin name = IO_L9P_T1_DQS_14,					Sch name = CRAM_DQ14
#set_property PACKAGE_PIN N17 [get_ports {DATA_SDRAM[14]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[14]}]
##Bank = 14, Pin name = IO_L12P_T1_MRCC_14,					Sch name = CRAM_DQ15
#set_property PACKAGE_PIN P17 [get_ports {DATA_SDRAM[15]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {DATA_SDRAM[15]}]

##Bank = 15, Pin name = IO_L23N_T3_FWE_B_15,					Sch name = CRAM_A0
#set_property PACKAGE_PIN J18 [get_ports {ADDR_SDRAM[1]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[1]}]
##Bank = 15, Pin name = IO_L18P_T2_A24_15,					Sch name = CRAM_A1
#set_property PACKAGE_PIN H17 [get_ports {ADDR_SDRAM[2]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[2]}]
##Bank = 15, Pin name = IO_L19N_T3_A21_VREF_15,				Sch name = CRAM_A2
#set_property PACKAGE_PIN H15 [get_ports {ADDR_SDRAM[3]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[3]}]
##Bank = 15, Pin name = IO_L23P_T3_FOE_B_15,					Sch name = CRAM_A3
#set_property PACKAGE_PIN J17 [get_ports {ADDR_SDRAM[4]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[4]}]
##Bank = 15, Pin name = IO_L13P_T2_MRCC_15,					Sch name = CRAM_A4
#set_property PACKAGE_PIN H16 [get_ports {ADDR_SDRAM[5]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[5]}]
##Bank = 15, Pin name = IO_L24P_T3_RS1_15,					Sch name = CRAM_A5
#set_property PACKAGE_PIN K15 [get_ports {ADDR_SDRAM[6]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[6]}]
##Bank = 15, Pin name = IO_L17P_T2_A26_15,					Sch name = CRAM_A6
#set_property PACKAGE_PIN K13 [get_ports {ADDR_SDRAM[7]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[7]}]
##Bank = 14, Pin name = IO_L11P_T1_SRCC_14,					Sch name = CRAM_A7
#set_property PACKAGE_PIN N15 [get_ports {ADDR_SDRAM[8]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[8]}]
##Bank = 14, Pin name = IO_L16N_T2_SRCC-14,					Sch name = CRAM_A8
#set_property PACKAGE_PIN V16 [get_ports {ADDR_SDRAM[9]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[9]}]
##Bank = 14, Pin name = IO_L22P_T3_A05_D21_14,				Sch name = CRAM_A9
#set_property PACKAGE_PIN U14 [get_ports {ADDR_SDRAM[10]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[10]}]
##Bank = 14, Pin name = IO_L22N_T3_A04_D20_14,				Sch name = CRAM_A10
#set_property PACKAGE_PIN V14 [get_ports {ADDR_SDRAM[11]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[11]}]
##Bank = 14, Pin name = IO_L20N_T3_A07_D23_14,				Sch name = CRAM_A11
#set_property PACKAGE_PIN V12 [get_ports {ADDR_SDRAM[12]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[12]}]
##Bank = 14, Pin name = IO_L8N_T1_D12_14,					Sch name = CRAM_A12
#set_property PACKAGE_PIN P14 [get_ports {ADDR_SDRAM[13]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[13]}]
##Bank = 14, Pin name = IO_L18P_T2_A12_D28_14,				Sch name = CRAM_A13
#set_property PACKAGE_PIN U16 [get_ports {ADDR_SDRAM[14]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[14]}]
##Bank = 14, Pin name = IO_L13N_T2_MRCC_14,					Sch name = CRAM_A14
#set_property PACKAGE_PIN R15 [get_ports {ADDR_SDRAM[15]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[15]}]
##Bank = 14, Pin name = IO_L8P_T1_D11_14,					Sch name = CRAM_A15
#set_property PACKAGE_PIN N14 [get_ports {ADDR_SDRAM[16]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[16]}]
##Bank = 14, Pin name = IO_L11N_T1_SRCC_14,					Sch name = CRAM_A16
#set_property PACKAGE_PIN N16 [get_ports {ADDR_SDRAM[17]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[17]}]
##Bank = 14, Pin name = IO_L6N_T0_D08_VREF_14,				Sch name = CRAM_A17
#set_property PACKAGE_PIN M13 [get_ports {ADDR_SDRAM[18]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[18]}]
##Bank = 14, Pin name = IO_L18N_T2_A11_D27_14,				Sch name = CRAM_A18
#set_property PACKAGE_PIN V17 [get_ports {ADDR_SDRAM[19]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[19]}]
##Bank = 14, Pin name = IO_L17P_T2_A14_D30_14,				Sch name = CRAM_A19
#set_property PACKAGE_PIN U17 [get_ports {ADDR_SDRAM[20]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[20]}]
##Bank = 14, Pin name = IO_L24N_T3_A00_D16_14,				Sch name = CRAM_A20
#set_property PACKAGE_PIN T10 [get_ports {ADDR_SDRAM[21]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[21]}]
##Bank = 14, Pin name = IO_L10P_T1_D14_14,					Sch name = CRAM_A21
#set_property PACKAGE_PIN M16 [get_ports {ADDR_SDRAM[22]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[22]}]
##Bank = 14, Pin name = IO_L23N_T3_A02_D18_14,				Sch name = CRAM_A22
#set_property PACKAGE_PIN U13 [get_ports {ADDR_SDRAM[23]}]
#	set_property IOSTANDARD LVCMOS33 [get_ports {ADDR_SDRAM[23]}]

#OFFSET = IN 10 ns VALID 10 ns BEFORE "CLK_IN" RISING;
#set_input_delay -clock CLK_OUT5_CLOCKMANAGER 2 [all_inputs]

#OFFSET = OUT 10 ns AFTER "CLK_IN";
#set_output_delay -clock CLK_OUT5_CLOCKMANAGER 10 [all_outputs]

# unconstrain paths between system clock and VGA clock
set_false_path -from CLK_OUT5_CLOCKMANAGER -to CLK_OUT1_CLOCKMANAGER
set_false_path -from CLK_OUT1_CLOCKMANAGER -to CLK_OUT5_CLOCKMANAGER
set_false_path -from CLK_OUT5_CLOCKMANAGER -to CLK_OUT2_CLOCKMANAGER
set_false_path -from CLK_OUT2_CLOCKMANAGER -to CLK_OUT5_CLOCKMANAGER
set_false_path -from CLK_OUT5_CLOCKMANAGER -to CLK_OUT3_CLOCKMANAGER
set_false_path -from CLK_OUT3_CLOCKMANAGER -to CLK_OUT5_CLOCKMANAGER
set_false_path -from CLK_OUT5_CLOCKMANAGER -to CLK_OUT4_CLOCKMANAGER
set_false_path -from CLK_OUT4_CLOCKMANAGER -to CLK_OUT5_CLOCKMANAGER

# unconstrainn paths within VGA clock
set_false_path -from CLK_OUT1_CLOCKMANAGER -to CLK_OUT1_CLOCKMANAGER
set_false_path -from CLK_OUT2_CLOCKMANAGER -to CLK_OUT2_CLOCKMANAGER
set_false_path -from CLK_OUT3_CLOCKMANAGER -to CLK_OUT3_CLOCKMANAGER
set_false_path -from CLK_OUT4_CLOCKMANAGER -to CLK_OUT4_CLOCKMANAGER

# unconstrain paths that do not actually exist to avoid false timing failure
set_false_path -from CLK_OUT1_CLOCKMANAGER -to CLK_OUT2_CLOCKMANAGER
set_false_path -from CLK_OUT1_CLOCKMANAGER -to CLK_OUT3_CLOCKMANAGER
set_false_path -from CLK_OUT1_CLOCKMANAGER -to CLK_OUT4_CLOCKMANAGER
set_false_path -from CLK_OUT2_CLOCKMANAGER -to CLK_OUT1_CLOCKMANAGER
set_false_path -from CLK_OUT2_CLOCKMANAGER -to CLK_OUT3_CLOCKMANAGER
set_false_path -from CLK_OUT2_CLOCKMANAGER -to CLK_OUT4_CLOCKMANAGER
set_false_path -from CLK_OUT3_CLOCKMANAGER -to CLK_OUT1_CLOCKMANAGER
set_false_path -from CLK_OUT3_CLOCKMANAGER -to CLK_OUT2_CLOCKMANAGER
set_false_path -from CLK_OUT3_CLOCKMANAGER -to CLK_OUT4_CLOCKMANAGER
set_false_path -from CLK_OUT4_CLOCKMANAGER -to CLK_OUT1_CLOCKMANAGER
set_false_path -from CLK_OUT4_CLOCKMANAGER -to CLK_OUT2_CLOCKMANAGER
set_false_path -from CLK_OUT4_CLOCKMANAGER -to CLK_OUT3_CLOCKMANAGER

set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 4 [current_design]
