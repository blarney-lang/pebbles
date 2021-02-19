// Copyright (c) 2016-2017 Alex Forencich
// Copyright (c) 2017 Matthew Naylor
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

module Golden_top(
  input wire [3:0] BUTTON,
  input wire CPU_RESET_n,

  output wire CLOCK_SCL,
  inout wire CLOCK_SDA,

  output wire [15:0] DDR3A_A,
  output wire [2:0] DDR3A_BA,
  output wire DDR3A_CAS_n,
  output wire [1:0] DDR3A_CK,
  output wire [1:0] DDR3A_CKE,
  output wire [0:0] DDR3A_CK_n,
  output wire [1:0] DDR3A_CS_n,
  output wire [7:0] DDR3A_DM,
  inout wire [63:0] DDR3A_DQ,
  inout wire [7:0] DDR3A_DQS,
  inout wire [7:0] DDR3A_DQS_n,
  input wire DDR3A_EVENT_n,
  output wire [1:0] DDR3A_ODT,
  output wire DDR3A_RAS_n,
  output wire DDR3A_RESET_n,
  output wire DDR3A_SCL,
  inout wire DDR3A_SDA,
  output wire DDR3A_WE_n,

  output wire [15:0] DDR3B_A,
  output wire [2:0] DDR3B_BA,
  output wire DDR3B_CAS_n,
  output wire [1:0] DDR3B_CK,
  output wire [1:0] DDR3B_CKE,
  output wire [0:0] DDR3B_CK_n,
  output wire [1:0] DDR3B_CS_n,
  output wire [7:0] DDR3B_DM,
  inout wire [63:0] DDR3B_DQ,
  inout wire [7:0] DDR3B_DQS,
  inout wire [7:0] DDR3B_DQS_n,
  input wire DDR3B_EVENT_n,
  output wire [1:0] DDR3B_ODT,
  output wire DDR3B_RAS_n,
  output wire DDR3B_RESET_n,
  output wire DDR3B_SCL,
  inout wire DDR3B_SDA,
  output wire DDR3B_WE_n,

  inout wire FAN_CTRL,

  output wire FLASH_ADV_n,
  output wire [1:0] FLASH_CE_n,
  output wire FLASH_CLK,
  output wire FLASH_OE_n,
  input wire [1:0] FLASH_RDY_BSY_n,
  output wire FLASH_RESET_n,
  output wire FLASH_WE_n,

  output wire [26:0] FSM_A,
  inout wire  [31:0] FSM_D,

  output wire [6:0] HEX0_D,
  output wire HEX0_DP,

  output wire [6:0] HEX1_D,
  output wire HEX1_DP,

  output wire [3:0] LED,
  output wire [3:0] LED_BRACKET,
  output wire LED_RJ45_L,
  output wire LED_RJ45_R,

  input wire OSC_50_B3B,
  input wire OSC_50_B3D,
  input wire OSC_50_B4A,
  input wire OSC_50_B4D,
  input wire OSC_50_B7A,
  input wire OSC_50_B7D,
  input wire OSC_50_B8A,
  input wire OSC_50_B8D,

  output wire PLL_SCL,
  inout wire PLL_SDA,

  output wire RS422_DE,
  input wire RS422_DIN,
  output wire RS422_DOUT,
  output wire RS422_RE_n,
  output wire RS422_TE,

  input wire RZQ_0,
  input wire RZQ_1,
  input wire RZQ_4,
  input wire RZQ_5,

/*
  input wire SFP_REFCLK_p,

  input wire SFPA_LOS,
  input wire SFPA_MOD0_PRSNT_n,
  output wire SFPA_MOD1_SCL,
  inout wire SFPA_MOD2_SDA,
  output wire [1:0] SFPA_RATESEL,
  input wire SFPA_RX_p,
  output wire SFPA_TXDISABLE,
  input wire SFPA_TXFAULT,
  output wire SFPA_TX_p,

  input wire SFPB_LOS,
  input wire SFPB_MOD0_PRSNT_n,
  output wire SFPB_MOD1_SCL,
  inout wire SFPB_MOD2_SDA,
  output wire [1:0] SFPB_RATESEL,
  input wire SFPB_RX_p,
  output wire SFPB_TXDISABLE,
  input wire SFPB_TXFAULT,
  output wire SFPB_TX_p,

  input wire SFPC_LOS,
  input wire SFPC_MOD0_PRSNT_n,
  output wire SFPC_MOD1_SCL,
  inout wire SFPC_MOD2_SDA,
  output wire [1:0] SFPC_RATESEL,
  input wire SFPC_RX_p,
  output wire SFPC_TXDISABLE,
  input wire SFPC_TXFAULT,
  output wire SFPC_TX_p,

  input wire SFPD_LOS,
  input wire SFPD_MOD0_PRSNT_n,
  output wire SFPD_MOD1_SCL,
  inout wire SFPD_MOD2_SDA,
  output wire [1:0] SFPD_RATESEL,
  input wire SFPD_RX_p,
  output wire SFPD_TXDISABLE,
  input wire SFPD_TXFAULT,
  output wire SFPD_TX_p,
*/

/*
  output wire [20:0] QDRIIA_A,
  output wire [1:0] QDRIIA_BWS_n,
  input wire QDRIIA_CQ_n,
  input wire QDRIIA_CQ_p,
  output wire [17:0] QDRIIA_D,
  output wire QDRIIA_DOFF_n,
  output wire QDRIIA_K_n,
  output wire QDRIIA_K_p,
  output wire QDRIIA_ODT,
  input wire [17:0] QDRIIA_Q,
  input wire QDRIIA_QVLD,
  output wire QDRIIA_RPS_n,
  output wire QDRIIA_WPS_n,

  output wire [20:0] QDRIIB_A,
  output wire [1:0] QDRIIB_BWS_n,
  input wire QDRIIB_CQ_n,
  input wire QDRIIB_CQ_p,
  output wire [17:0] QDRIIB_D,
  output wire QDRIIB_DOFF_n,
  output wire QDRIIB_K_n,
  output wire QDRIIB_K_p,
  output wire QDRIIB_ODT,
  input wire [17:0] QDRIIB_Q,
  input wire QDRIIB_QVLD,
  output wire QDRIIB_RPS_n,
  output wire QDRIIB_WPS_n,

  output wire [20:0] QDRIIC_A,
  output wire [1:0] QDRIIC_BWS_n,
  input wire QDRIIC_CQ_n,
  input wire QDRIIC_CQ_p,
  output wire [17:0] QDRIIC_D,
  output wire QDRIIC_DOFF_n,
  output wire QDRIIC_K_n,
  output wire QDRIIC_K_p,
  output wire QDRIIC_ODT,
  input wire [17:0] QDRIIC_Q,
  input wire QDRIIC_QVLD,
  output wire QDRIIC_RPS_n,
  output wire QDRIIC_WPS_n,

  output wire [20:0] QDRIID_A,
  output wire [1:0] QDRIID_BWS_n,
  input wire QDRIID_CQ_n,
  input wire QDRIID_CQ_p,
  output wire [17:0] QDRIID_D,
  output wire QDRIID_DOFF_n,
  output wire QDRIID_K_n,
  output wire QDRIID_K_p,
  output wire QDRIID_ODT,
  input wire [17:0] QDRIID_Q,
  input wire QDRIID_QVLD,
  output wire QDRIID_RPS_n,
  output wire QDRIID_WPS_n,
*/

  input wire SMA_CLKIN,
  output wire SMA_CLKOUT,

  input wire [3:0] SW,

  output wire TEMP_CLK,
  inout wire TEMP_DATA,
  input wire TEMP_INT_n,
  input wire TEMP_OVERT_n

);

wire clk_50mhz = OSC_50_B7A;
wire rst_50mhz = ~CPU_RESET_n;
wire rst_50mhz_n = CPU_RESET_n;

S5_DDR3_QSYS u0 (
  .clk_clk                                   (clk_50mhz),
  .reset_reset_n                             (rst_50mhz_n),

  // DRAMs
  .memory_mem_a                              (DDR3A_A),
  .memory_mem_ba                             (DDR3A_BA),
  .memory_mem_ck                             (DDR3A_CK),
  .memory_mem_ck_n                           (DDR3A_CK_n),
  .memory_mem_cke                            (DDR3A_CKE),
  .memory_mem_cs_n                           (DDR3A_CS_n),
  .memory_mem_dm                             (DDR3A_DM),
  .memory_mem_ras_n                          (DDR3A_RAS_n),
  .memory_mem_cas_n                          (DDR3A_CAS_n),
  .memory_mem_we_n                           (DDR3A_WE_n),
  .memory_mem_reset_n                        (DDR3A_RESET_n),
  .memory_mem_dq                             (DDR3A_DQ),
  .memory_mem_dqs                            (DDR3A_DQS),
  .memory_mem_dqs_n                          (DDR3A_DQS_n),
  .memory_mem_odt                            (DDR3A_ODT),
  .oct_rzqin                                 (RZQ_4),
  //.mem_if_ddr3_emif_status_local_init_done   (ddr3_local_init_done),   
  //.mem_if_ddr3_emif_status_local_cal_success (ddr3_local_cal_success), 
  //.mem_if_ddr3_emif_status_local_cal_fail    (ddr3_local_cal_fail),

  .memory_2_mem_a                              (DDR3B_A),
  .memory_2_mem_ba                             (DDR3B_BA),
  .memory_2_mem_ck                             (DDR3B_CK),
  .memory_2_mem_ck_n                           (DDR3B_CK_n),
  .memory_2_mem_cke                            (DDR3B_CKE),
  .memory_2_mem_cs_n                           (DDR3B_CS_n),
  .memory_2_mem_dm                             (DDR3B_DM),
  .memory_2_mem_ras_n                          (DDR3B_RAS_n),
  .memory_2_mem_cas_n                          (DDR3B_CAS_n),
  .memory_2_mem_we_n                           (DDR3B_WE_n),
  .memory_2_mem_reset_n                        (DDR3B_RESET_n),
  .memory_2_mem_dq                             (DDR3B_DQ),
  .memory_2_mem_dqs                            (DDR3B_DQS),
  .memory_2_mem_dqs_n                          (DDR3B_DQS_n),
  .memory_2_mem_odt                            (DDR3B_ODT),
  //.mem_if_ddr3_emif_2_status_local_init_done   (ddr3_2_local_init_done),
  //.mem_if_ddr3_emif_2_status_local_cal_success (ddr3_2_local_cal_success),
  //.mem_if_ddr3_emif_2_status_local_cal_fail    (ddr3_2_local_cal_fail),
);

endmodule 
