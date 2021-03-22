module DE10_Pro(
  input CLK_100_B3I,
  input CLK_50_B2C,
  input CLK_50_B2L,
  input CLK_50_B3C,
  input CLK_50_B3I,
  input CLK_50_B3L,

  input CPU_RESET_n,
  input [1:0] BUTTON,
  input [1:0] SW,
  output [3:0] LED,

  output FLASH_CLK,
  output [27:1] FLASH_A,
  inout [15:0] FLASH_D,
  output FLASH_CE_n,
  output FLASH_WE_n,
  output FLASH_OE_n,
  output FLASH_ADV_n,
  output FLASH_RESET_n,
  input FLASH_RDY_BSY_n,

  input DDR4A_REFCLK_p,
  output [16:0] DDR4A_A,
  output [1:0] DDR4A_BA,
  output [1:0] DDR4A_BG,
  output DDR4A_CK,
  output DDR4A_CK_n,
  output DDR4A_CKE,
  inout [8:0] DDR4A_DQS,
  inout [8:0] DDR4A_DQS_n,
  inout [71:0] DDR4A_DQ,
  inout [8:0] DDR4A_DBI_n,
  output DDR4A_CS_n,
  output DDR4A_RESET_n,
  output DDR4A_ODT,
  output DDR4A_PAR,
  input DDR4A_ALERT_n,
  output DDR4A_ACT_n,
  input DDR4A_EVENT_n,
  inout DDR4A_SCL,
  inout DDR4A_SDA,
  input DDR4A_RZQ,

  input EXP_EN,

  inout UFL_CLKIN_p,
  inout UFL_CLKIN_n
);

wire reset_n;
wire ddr4_local_reset_req;

wire ddr4_a_local_reset_done;
wire ddr4_a_status_local_cal_fail;
wire ddr4_a_status_local_cal_success;

wire [11:0] ddr4_status;

// Reset release
wire ninit_done;
reset_release reset_release (
        .ninit_done(ninit_done)
        );

assign reset_n = !ninit_done && CPU_RESET_n;
assign ddr4_status =
  {ddr4_a_status_local_cal_fail,
     ddr4_a_status_local_cal_success,
       ddr4_a_local_reset_done};

DE10_Pro_QSYS DE10_Pro_QSYS_inst (
		.clk_clk(CLK_50_B3I),                                             
		.reset_reset(~reset_n),                                          

		.iopll_0_locked_export(),                               

		//.ddr4_local_reset_req_external_connection_export(ddr4_local_reset_req),
		//.ddr4_status_external_connection_export(ddr4_status),
		//.emif_s10_ddr4_a_local_reset_req_local_reset_req(ddr4_local_reset_req),
		//.emif_s10_ddr4_a_local_reset_status_local_reset_done(
    //    ddr4_a_local_reset_done), 

		.emif_s10_ddr4_a_mem_mem_ck(DDR4A_CK),                          
		.emif_s10_ddr4_a_mem_mem_ck_n(DDR4A_CK_n),                        
		.emif_s10_ddr4_a_mem_mem_a(DDR4A_A),                           
		.emif_s10_ddr4_a_mem_mem_act_n(DDR4A_ACT_n),                       
		.emif_s10_ddr4_a_mem_mem_ba(DDR4A_BA),                          
		.emif_s10_ddr4_a_mem_mem_bg(DDR4A_BG),                          
		.emif_s10_ddr4_a_mem_mem_cke(DDR4A_CKE),                         
		.emif_s10_ddr4_a_mem_mem_cs_n(DDR4A_CS_n),                        
		.emif_s10_ddr4_a_mem_mem_odt(DDR4A_ODT),                         
		.emif_s10_ddr4_a_mem_mem_reset_n(DDR4A_RESET_n),                     
		.emif_s10_ddr4_a_mem_mem_par(DDR4A_PAR),                         
		.emif_s10_ddr4_a_mem_mem_alert_n(DDR4A_ALERT_n),                     
		.emif_s10_ddr4_a_mem_mem_dqs(DDR4A_DQS),                         
		.emif_s10_ddr4_a_mem_mem_dqs_n(DDR4A_DQS_n),                       
		.emif_s10_ddr4_a_mem_mem_dq(DDR4A_DQ),                          
		.emif_s10_ddr4_a_mem_mem_dbi_n(DDR4A_DBI_n),                       
		.emif_s10_ddr4_a_oct_oct_rzqin(DDR4A_RZQ),                       
		.emif_s10_ddr4_a_pll_ref_clk_clk(DDR4A_REFCLK_p),                     
		.emif_s10_ddr4_a_status_local_cal_success(ddr4_a_status_local_cal_success),
		.emif_s10_ddr4_a_status_local_cal_fail(ddr4_a_status_local_cal_fail)
	);

endmodule
