module bram_2p(
    clk,
    read_en,
    read_addr,
    read_val,
    write_en,
    write_addr,
    write_val
);
    input clk;
    input read_en;
    input [9:0] read_addr;
    output reg [31:0] read_val;
    input write_en;
    input [9:0] write_addr;
    input [31:0] write_val;

    reg [31:0] read_val_tmp;

    (* ram_style = "block" *)
    reg [31:0] ram[0:1023];

    always @(posedge clk) begin
        if (read_en) begin
            read_val_tmp <= ram[read_addr];
        end
        read_val <= read_val_tmp;

        if (write_en) begin
            ram[write_addr] <= write_val;
        end
    end

endmodule