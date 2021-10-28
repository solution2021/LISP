// Next available MSG number is   369
// MODULE_ID DDMODIFY_DCL_
//
//     DDMODIFY.DCL - dialog declarations for DDMODIFY
//
//     Copyright 1996 by Autodesk, Inc.
//
//     Permission to use, copy, modify, and distribute this software
//     for any purpose and without fee is hereby granted, provided
//     that the above copyright notice appears in all copies and
//     that both that copyright notice and the limited warranty and
//     restricted rights notice below appear in all supporting
//     documentation.
//
//     AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
//     AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
//     MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
//     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
//     UNINTERRUPTED OR ERROR FREE.
//
//     Use, duplication, or disclosure by the U.S. Government is subject to
//     restrictions set forth in FAR 52.227-19 (Commercial Computer
//     Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
//     (Rights in Technical Data and Computer Software), as applicable.
//
//.
//
//     Dialogue for the DDMODIFY command, for use with DDMODIFY.LSP


//dcl_settings : default_dcl_settings { audit_level = 3; }


//-------- Subassemblies and prototypes shared across several dialogues -------

common_fields : column {
    : boxed_row {
        label = "Properties";
        : column {
            : row {
                fixed_width = true;
                : button {
                    label = "Color...";
                    mnemonic = "C";
                    key = "b_color";
                    width = 15;
                    fixed_width = true;
                }
                : image_button {
                    key = "show_image";
                    height = 1;
                    width = 3;
                }
                : text {
                    key = "t_color";
                    width = 12;
                }
            }
            : row {
                fixed_width = true;
                : button {
                    label = "Layer...";
                    mnemonic = "L";
                    key = "b_name";
                    width = 15;
                    fixed_width = true;
                }
                : text {
                    key = "t_layer";
                    width = 16;
                }
            }
            : row {
                fixed_width = true;
                : button {
                    label = "Linetype...";
                    mnemonic = "i";
                    key = "b_line";
                    width = 15;
                }
                : text {
                    key = "t_ltype";
                    width = 16;
                }
           }
        }
        spacer_1;
        : column {
            fixed_width = true;
            handle_assembly;
            : row {
                : text_part {
                    label = "Thickness:";
                    mnemonic = "T";
                    key = "b_thickness";
                    width = 16;
                    fixed_width = true;
                }
                : edit_box {
                    key = "eb_thickness";
                    edit_width = 15;
                }
            }
            : row {
                : text_part {
                    label = "Linetype Scale:";
                    mnemonic = "S";
                    width = 16;
                    fixed_width = true;
                }
                : edit_box {
                    key = "eb_ltscale";
                    edit_width = 15;
                }
            }
        }
    }
    spacer;
}

number_box : edit_box {
    edit_width = 10;
}

pick_point_button : button {
    label = "Pick Point <";
    alignment = centered;
}

pick_point_1_button : pick_point_button {
    key = "pick_1";
    mnemonic = "P";
}
pick_point_2_button : pick_point_button {
    key = "pick_2";
    mnemonic = "k";
}
pick_point_3_button : pick_point_button {
    key = "pick_3";
    mnemonic = "o";
}
pick_point_4_button : pick_point_button {
    key = "pick_4";
    mnemonic = "n";
}

x_box : number_box {
    label = "X:";
    mnemonic = "X";
}
y_box : number_box {
    label = "Y:";
    mnemonic = "Y";
}
z_box : number_box {
    label = "Z:";
    mnemonic = "Z";
}

x1_box : x_box {
    key = "x1_pt";
}
x2_box : x_box {
    key = "x2_pt";
}
x3_box : x_box {
    key = "x3_pt";
}
x4_box : x_box {
    key = "x4_pt";
}

y1_box : y_box {
    key = "y1_pt";
}
y2_box : y_box {
    key = "y2_pt";
}
y3_box : y_box {
    key = "y3_pt";
}
y4_box : y_box {
    key = "y4_pt";
}

z1_box : z_box {
    key = "z1_pt";
}
z2_box : z_box {
    key = "z2_pt";
}
z3_box : z_box {
    key = "z3_pt";
}
z4_box : z_box {
    key = "z4_pt";
}
// Xline/Ray edit boxes.
xline_x1 : x_box {
    key = "xline_x1";
}
xline_y1 : y_box {
    key = "xline_y1";
}
xline_z1 : z_box {
    key = "xline_z1";
}
xline_x2 : x_box {
    key = "xline_x2";
}
xline_y2 : y_box {
    key = "xline_y2";
}
xline_z2 : z_box {
    key = "xline_z2";
}


handle_assembly : row {
    : text {
        label = "Handle:";
        width = 16;
        fixed_width = true;
    }
    // width = width of thickness/ltscale edit boxes - 3;
    : text {
        key = "Handle";
        width = 12;
    }
}

dimstyle_buttons :column {
    :column{
        fixed_width = true;
        fixed_height = true;
        alignment = centered;
        :button{
            label = "Geometry...";
            mnemonic = "G";
            key = "mod_geom";
            width = 13;
        }
        :button{
            label = "Format...";
            mnemonic = "F";
            key = "mod_format";
            width = 13;
        }
        :button{
            label = "Annotation...";
            mnemonic = "A";
            key = "mod_annot";
            width = 13;
        }
    }
}

dimedit_column :column {
    :column{
        fixed_width = true;
        fixed_height = true;
        :button{
            label = "Edit...";
            mnemonic = "d";
            key = "mod_text";
            height = 2;
        }
        :popup_list{
            label = "Style: ";
            mnemonic = "e";
            key = "mod_style";
            width = 32;
            list = "";
        }
    }
}


//-------------------- Dialogues --------------------
ddmline : dialog {
    label = "Modify Multiline";
    common_fields;
    : row {
      : concatenation {
         : text_part {
             label = "MLine Style: ";
             width = 12;
         }
         : text_part {
             key = "ml_style";
             width = 33;
             fixed_width = true;
         }
      }
      spacer_1;
    }
    spacer;
    : row { 
        alignment = centered;
        fixed_width = true;
        ok_cancel_help;
    }
    errtile;
}

ddxline : dialog {
    label = "Modify Xline";
    common_fields;
    spacer;
    : row {
        : boxed_column {
            label = "Root Point";
            fixed_width = true;
            pick_point_1_button;
            xline_x1;
            xline_y1;
            xline_z1;
        }
        : boxed_column {
            label = "Second Point";
            fixed_width = true;
            pick_point_2_button;
            xline_x2;
            xline_y2;
            xline_z2;
        }
        : boxed_column {
            label = "Direction Vector";
               spacer_1;
                : concatenation {
                    : text_part {
                        label = " X:";
                        width = 4;
                    }
                    : text_part {
                        key = "dir_x";
                        width = 10;
                    }
                }
                : concatenation {
                    : text_part {
                        label = " Y:";
                        width = 4;
                    }
                    : text_part {
                        key = "dir_y";
                        width = 10;
                    }
                }
                : concatenation {
                    : text_part {
                        label = " Z:";
                        width = 4;
                    }
                    : text_part {
                        key = "dir_z";
                        width = 10;
                    }
                }
                spacer_1;
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddray : dialog {
    label = "Modify Ray";
    common_fields;
    spacer;
    : row {
        : boxed_column {
            label = "Start Point";
            fixed_width = true;
            pick_point_1_button;
            xline_x1;
            xline_y1;
            xline_z1;
        }
        : boxed_column {
            label = "Second Point";
            fixed_width = true;
            pick_point_2_button;
            xline_x2;
            xline_y2;
            xline_z2;
        }
        : boxed_column {
            label = "Direction Vector";
               spacer_1;
                : concatenation {
                    : text_part {
                        label = " X:";
                        width = 4;
                    }
                    : text_part {
                        key = "dir_x";
                        width = 10;
                    }
                }
                : concatenation {
                    : text_part {
                        label = " Y:";
                        width = 4;
                    }
                    : text_part {
                        key = "dir_y";
                        width = 10;
                    }
                }
                : concatenation {
                    : text_part {
                        label = " Z:";
                        width = 4;
                    }
                    : text_part {
                        key = "dir_z";
                        width = 10;
                    }
                }
                spacer_1;
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddpoint : dialog {
    label = "Modify Point";
    common_fields;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "Location";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        spacer;
    }
    ok_cancel_help_errtile;
}

dd3dsolid : dialog {
    label = "Modify 3DSolid";
    common_fields;
    : row {
        fixed_width = true;
        spacer;
        : column {
            alignment = top;
            fixed_height = true;
            spacer_1;
        }
    }
    ok_cancel_help_errtile;
}
ddregion : dialog {
    label = "Modify Region";
    common_fields;
    : row {
        fixed_width = true;
        spacer;
        : column {
            alignment = top;
            fixed_height = true;
            spacer_1;
        }
    }
    ok_cancel_help_errtile;
}
ddbody : dialog {
    label = "Modify Body";
    common_fields;
    : row {
        fixed_width = true;
        spacer;
        : column {
            alignment = top;
            fixed_height = true;
            spacer_1;
        }
    }
    ok_cancel_help_errtile;
}
ddline : dialog {
    label = "Modify Line";
    common_fields;
    : row {
        : boxed_column {
            label = "From Point";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        : boxed_column {
            label = "To Point";
            fixed_width = true;
            pick_point_2_button;
            x2_box;
            y2_box;
            z2_box;
        }
        : column {
            : text {
                label = "Delta XYZ: ";
            }
            : column {
                fixed_height = true;
                : concatenation {
                    : text_part {
                        label = " X:";
                        width = 4;
                    }
                    : text_part {
                        key = "delta_x";
                        width = 14;
                    }
                }
                : concatenation {
                    : text_part {
                        label = " Y:";
                        width = 4;
                    }
                    : text_part {
                        key = "delta_y";
                        width = 14;
                    }
                }
                : concatenation {
                    : text_part {
                        label = " Z:";
                        width = 4;
                    }
                    : text_part {
                        key = "delta_z";
                        width = 14;
                    }
                }
            }
            spacer_0;
            : column {
                fixed_height = true;
                : concatenation {
                    : text_part {
                        label = "Length: ";
                        width = 8;
                    }
                    : text_part {
                        key = "l_length";
                        width = 14;
                    }
                }
                fixed_height = true;
                : concatenation {
                    : text_part {
                        label = "Angle: ";
                        width = 8;
                    }
                    : text_part {
                        key = "l_angle";
                        width = 10;
                    }
                }
            }
        }
    spacer_1;
    }
    spacer;
    ok_cancel_help_errtile;
}

ddellipse : dialog {
    label = "Modify Ellipse";
    common_fields;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "Center";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        spacer;
        : column {
            fixed_height = true;
            : number_box {
                label = "Major Radius:";
                mnemonic = "M";
                key = "majrad";
            }
            : number_box {
                label = "Minor Radius:";
                mnemonic = "N";
                key = "minrad";
            }
            : row {
                : column {
                    : text {
                        label = "Radius Ratio:";
                    }
                }
                : column {
                    : text {
                        key = "rratio";
                        width = 11;
                    }
                }
            } 
            : number_box {
                label = "Start Angle:";
                mnemonic = "S";
                key = "st_ang";
            }
            : number_box {
                label = "End Angle:";
                mnemonic = "E";
                key = "end_eang";
            }
        }
        : column {
            : boxed_column {
                label = "Major Axis Vector";
                fixed_width = true;
                : row {
                    : column {
                        : text {
                            label = "X:";
                        }
                        : text {
                            label = "Y:";
                        }
                        : text {
                            label = "Z:";
                        }
                    }
                    : column {
                        : text {
                            key = "Majraddirx";
                            width = 11;
                        }
                        : text {
                            key = "Majraddiry";
                            width = 11;
                        }
                        : text {
                            key = "Majraddirz";
                            width = 11;
                        }
                    }
                }
            }
            : column {
                fixed_height = true;
                : row {
                    : column {
                        : text {
                            label = "Area: ";
                            key = "Area_text";
                        }
                    }
                    : column {
                        : text {
                            key = "Area";
                            width = 20;
                        }
                    }
                }
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddspline : dialog {
    label = "Modify Spline";
    common_fields;
    : row {
        : boxed_column {
            label = "Control Points";
            fixed_width = true;
            : row {
                : column {
                    vertical_margin = wide;
                    : concatenation {
                        : text_part {
                            label = "Vertex:";
                        }
                        : text_part {
                            key = "cntl_ctr";
                            width = 4;
                        }
                    }
                }
                : column {
                    : button {
                        label = "Next";
                        mnemonic = "N";
                        key = "next_cntlpt";
                    }
                }
            }
            : concatenation {
                : text_part {
                  label = "X: ";
                }
                : text_part {
                  key = "xtext";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Y: ";
                }
                : text_part {
                  key = "ytext";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Z: ";
                }
                : text_part {
                  key = "ztext";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Weight: ";
                  key = "weight_text";
                }
                : text_part {
                  key = "weight";
                  width = 10;
                }
            }
        }
        : column {
            fixed_height = true;
            : row {
               : column {
                   : text {
                       label = "      ";
                   }
                   : text {
                       label = "Degree:";
                   }
                   : text {
                       label = "Properties";
                   }
                   : text {
                       label = "      ";
                   }
                   : text {
                       label = "      ";
                   }
                   : text {
                       label = "      ";
                   }
                   : text {
                       label = "      ";
                   }
               }
               : column {
                   : text {
                       label = "    ";
                   }
                   : text {
                       key = "Degree";
                       width = 11;
                   }
                   : text {
                       key = "SpProp1";
                       width = 12;
                   }
                   : text {
                       key = "SpProp2";
                       width = 12;
                   }
                   : text {
                       key = "SpProp3";
                       width = 12;
                   }
                   : text {
                       key = "SpProp4";
                       width = 12;
                   }
                   : text {
                       key = "SpProp5";
                       width = 12;
                   }
               }
            }
        }
        : boxed_column {
            label = "Data Points";
            fixed_width = true;
            key = "data_pts";
            : row {
                : column {
                    vertical_margin = wide;
                    : concatenation {
                        : text_part {
                            label = "Vertex:";
                        }
                        : text_part {
                            key = "data_ctr";
                            width = 4;
                        }
                    }    
                }
                : column {
                    : button {
                        label = "Next";
                        mnemonic = "N";
                        key = "next_datapt";
                    }
                }
            }    
            : concatenation {
                : text_part {
                  label = "X: ";
                }
                : text_part {
                  key = "dxtext";
                  width = 10;  
                }
            }    
            : concatenation {
                : text_part {
                  label = "Y: ";
                }
                : text_part {
                  key = "dytext";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Z: ";
                }
                : text_part {
                  key = "dztext";
                  width = 10;
                }
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddcircle : dialog {
    label = "Modify Circle";
    common_fields;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "Center";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        spacer;
        : column {
            fixed_height = true;
            : text {
               label = "   ";
            }
            : number_box {
                label = "Radius:";
                mnemonic = "R";
                key = "radius";
            }
            : row {
                : column {
                    : text {
                        label = "Diameter:";
                    }
                    : text {
                        label = "Circumference:";
                    }
                    : text {
                        label = "Area: ";
                    }
                }
                : column {
                    : text {
                        key = "Dia";
                        width = 11;
                        alignment = right;
                    }
                    : text {
                        key = "Circum";
                        width = 11;
                        alignment = right;
                    }
                    : text {
                        key = "Area";
                        width = 20;
                        alignment = right;
                    }
                }
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddarc : dialog {
    label = "Modify Arc";
    common_fields;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "Center";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        spacer;
        : column {
            alignment = top;
            fixed_width = true;
            : text {
               label = "   ";
            }
            : number_box {
                label = "Radius: ";
                mnemonic = "R";
                key = "radius";
            }
            : number_box {
                label = "Start Angle: ";
                mnemonic = "A";
                key = "st_ang";
            }
            : number_box {
                label = "End Angle: ";
                mnemonic = "E";
                key = "end_ang";
            }
            : concatenation {
                : text_part {
                  label = "Total Angle: ";
                  width = 14;
                }
                : text_part {
                  key = "tot_angle";
                  width = 10;
                }
            }
        }
        spacer;
        : column {
            fixed_height = true;
            fixed_width = true;
            alignment = top;
            : text {
                label = "   ";
            }
            : spacer { height = 0.2; }
            : concatenation {
                : text_part {
                    label = "Arc Length: ";
                    width = 13;
                }
                : text_part {
                    key = "arclen";
                    width = 8;
                }
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

dd3dface : dialog {
    label = "Modify 3D Face";
    common_fields;
    : row {
        : boxed_column {
            label = "Point 1";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        : boxed_column {
            label = "Point 2";
            fixed_width = true;
            pick_point_2_button;
            x2_box;
            y2_box;
            z2_box;
        }
        : boxed_column {
            label = "Point 3";
            fixed_width = true;
            pick_point_3_button;
            x3_box;
            y3_box;
            z3_box;
        }
        : boxed_column {
            label = "Point 4";
            fixed_width = true;
            pick_point_4_button;
            x4_box;
            y4_box;
            z4_box;
        }
    }
    spacer;
    : boxed_row {
        label = "Visibility";
        spacer_0;
        : toggle {
          label = "Edge &1";
          key = "edge_1";
          fixed_width = true;
        }
        spacer_0;
        : toggle {
          label = "Edge &2";
          key = "edge_2";
          fixed_width = true;
        }
        spacer_0;
        : toggle {
          label = "Edge &3";
          key = "edge_3";
          fixed_width = true;
        }
        spacer_0;
        : toggle {
          label = "Edge &4";
          key = "edge_4";
          fixed_width = true;
        }
        spacer_0;
    }
    spacer;
    ok_cancel_help_errtile;
}

ddsolid : dialog {
    label = "Modify Solid";
    common_fields;
    : row {
        children_alignment = top;
        children_fixed_width = true;
        children_fixed_height = true;
        : column {
            : boxed_column {
                label = "Point 1";
                pick_point_1_button;
                x1_box;
                y1_box;
            }
        }
        : boxed_column {
            label = "Point 2";
            pick_point_2_button;
            x2_box;
            y2_box;
        }
        : boxed_column {
            label = "Point 3";
            pick_point_3_button;
            x3_box;
            y3_box;
        }
        : boxed_column {
            label = "Point 4";
            pick_point_4_button;
            x4_box;
            y4_box;
            z4_box;
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddtrace : dialog {
    label = "Modify Trace";
    common_fields;
    : row {
        children_alignment = top;
        children_fixed_width = true;
        children_fixed_height = true;
        : column {
            : boxed_column {
                label = "Point 1";
                pick_point_1_button;
                x1_box;
                y1_box;
            }
        }
        : boxed_column {
            label = "Point 2";
            pick_point_2_button;
            x2_box;
            y2_box;
        }
        : boxed_column {
            label = "Point 3";
            pick_point_3_button;
            x3_box;
            y3_box;
        }
        : boxed_column {
            label = "Point 4";
            pick_point_4_button;
            x4_box;
            y4_box;
            z4_box;
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddblock : dialog {
    label = "Modify Block Insertion";
    common_fields;
    : concatenation {
       : text_part {
           label = "Block Name: ";
           width = 12;
       }
       : text_part {
           key = "Bl_name";
           width = 33;
       }
    }
    spacer;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "At";
            fixed_width = true;
            fixed_height = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        spacer;
        : column {
            fixed_width = true;
            fixed_height = true;
            : text {
               label = "   ";
            }
            : number_box {
                label = "X-scale: ";
                mnemonic = "a";
                key = "xscale";
            }
            : number_box {
                label = "Y-scale: ";
                mnemonic = "l";
                key = "yscale";
            }
            : number_box {
                label = "Z-scale: ";
                mnemonic = "e";
                key = "zscale";
            }
            : number_box {
                label = "Rotation: ";
                mnemonic = "R";
                key = "rot";
            }
        }
        spacer;
        : column {
            fixed_width = true;
            fixed_height = true;
            : text {
               label = "   ";
            }
            : number_box {
                label = "Columns: ";
                mnemonic = "o";
                key = "columns";
            }
            : number_box {
                label = "Rows: ";
                mnemonic = "w";
                key = "rows";
            }
            : number_box {
                label = "Col Spacing: ";
                mnemonic = "n";
                key = "col_sp";
            }
            : number_box {
                label = "Row Spacing: ";
                mnemonic = "g";
                key = "row_sp";
            }
        }
    }
    : row {
        : toggle {
            label = "Show clipped &block";
            mnemonic = "b";
            key = "xcliponoff";
        }
    }
    ok_cancel_help_errtile;
}

ddhatch : dialog {
    label = "Modify Associative Hatch";
    common_fields;
    : row {
      : concatenation {
         : text_part {
             label = "Block Name: ";
             width = 12;
         }
         : text_part {
             key = "Bl_name";
             width = 33;
             fixed_width = true;
         }
      }
      spacer_1;
    }
    spacer;
    : row { 
        alignment = centered;
        fixed_width = true;
        ok_cancel;
        : spacer { width = 2; }
        : button {
            label = "Hatch Edit...";
            mnemonic = "H";
            key = "b_hatch";
        }
        : spacer { width = 2; }
        help_button;
    }
    errtile;
}

ddnewhatch : dialog {
    label = "Modify Hatch";
    common_fields;
    spacer;
    : row { 
        alignment = centered;
        fixed_width = true;
        ok_cancel;
        : spacer { width = 2; }
        : button {
            label = "Hatch Edit...";
            mnemonic = "H";
            key = "b_hatch";
        }
        : spacer { width = 2; }
        help_button;
    }
    errtile;
}

ddxref : dialog {
    label = "Modify External Reference";
    common_fields;
    : row {
        : concatenation {
           : text_part {
               label = "Xref Name: ";
           }
           : text_part {
               key = "Bl_name";
               width = 12;
           }
        }
        spacer_0;
        : concatenation {
           : text_part {
               label = "Path: ";
           }
           : text_part {
               key = "path";
               width = 35;
           }
        }
    }
    spacer;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "At";
            fixed_width = true;
            fixed_height = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        spacer;
        : column {
            fixed_width = true;
            fixed_height = true;
            : text {
               label = "   ";
            }
            : number_box {
                label = "X-scale: ";
                mnemonic = "a";
                key = "xscale";
            }
            : number_box {
                label = "Y-scale: ";
                mnemonic = "l";
                key = "yscale";
            }
            : number_box {
                label = "Z-scale: ";
                mnemonic = "e";
                key = "zscale";
            }
            : number_box {
                label = "Rotation: ";
                mnemonic = "R";
                key = "rot";
            }
        }
        spacer;
        : column {
            fixed_width = true;
            fixed_height = true;
            : text {
               label = "   ";
            }
            : number_box {
                label = "Columns: ";
                mnemonic = "o";
                key = "columns";
            }
            : number_box {
                label = "Rows: ";
                mnemonic = "w";
                key = "rows";
            }
            : number_box {
                label = "Col Spacing: ";
                mnemonic = "n";
                key = "col_sp";
            }
            : number_box {
                label = "Row Spacing: ";
                mnemonic = "g";
                key = "row_sp";
            }
        }
    }
    : row {
        : toggle {
            label = "Show clipped xre&f";
            mnemonic = "f";
            key = "xcliponoff";
        }
    }
    ok_cancel_help_errtile;
}

ddimage : dialog {
    label = "Modify Image";
    common_fields;
    : row {
        : concatenation {
           : text_part {
               label = "Image Name: ";
           }
           : text_part {
               key = "image_name";
               width = 12;
           }
        }
        spacer_0;
        : concatenation {
           : text_part {
               label = "Path: ";
           }
           : text_part {
               key = "image_path";
               width = 35;
           }
        }
    }
    : row {
        fixed_width = true;
        : boxed_column {
            label = "At";
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        spacer_0;
        : boxed_column {
            label = "Parameters";
            : number_box {
                label = "Sc&ale: ";
                mnemonic = "a";
                key = "xscale";
            }
            : number_box {
                label = "&Rotation: ";
                mnemonic = "r";
                key = "st_ang";
            }
            : number_box {
                label = "&Width: ";
                mnemonic = "w";
                key = "wid";
            }
            : number_box {
                label = "H&eight: ";
                mnemonic = "e";
                key = "hght";
            }
        }
        spacer_0;
        : boxed_column {
            label = "Display Properties";
            : toggle {
                label = "Show Ima&ge";
                mnemonic = "g";
                key = "image_show";
            }
            : toggle {
                label = "Show Non-&Ortho Image";
                mnemonic = "o";
                key = "image_non_ortho";
            }
            : toggle {
                label = "Show Clippe&d Image";
                mnemonic = "d";
                key = "image_clipped";
            }
            : toggle {
                label = "Tra&nsparency";
                mnemonic = "n";
                key = "image_transparency";
            }
            : button {
                label = "Ad&just Image";
                mnemonic = "j";
                key = "image_adjust";
            }
        }
    }
    ok_cancel_help_errtile;
}

ddshape : dialog {
    label = "Modify Shape";
    common_fields;
    : concatenation {
       : text_part {
           label = "Shape: ";
       }
       : text_part {
           key = "sh_name";
           width = 10;
       }
    }
    spacer;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "Origin";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        : column {
            fixed_width = true;
            : text {
               label = "   ";
            }
            : number_box {
                label = "Size: ";
                mnemonic = "e";
                key = "hght";
            }
            : number_box {
                label = "Rotation: ";
                mnemonic = "R";
                key = "rot";
            }
            : number_box {
                label = "Width Factor: ";
                mnemonic = "W";
                key = "wid";
            }
            : number_box {
                label = "Obliquing: ";
                mnemonic = "O";
                key = "obl";
            }
        }
        spacer;
        : column {
            alignment = top;
            fixed_height = true;
            : text {
               height = 1.4;
               label = "   ";
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddtext : dialog {
    label = "Modify Text";
    common_fields;
    : edit_box {
        label = "Text: ";
        mnemonic = "e";
        key = "t_string";
        width = 50;
        edit_limit = 2048;
    }
    spacer;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "Origin";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        : column {
            fixed_width = true;
            : text {
                label = "  ";
            }
            : number_box {
                label = "Height: ";
                mnemonic = "g";
                key = "hght";
            }
            : number_box {
                label = "Rotation: ";
                mnemonic = "R";
                key = "rot";
            }
            : number_box {
                label = "Width Factor: ";
                mnemonic = "W";
                key = "wid";
            }
            : number_box {
                label = "Obliquing: ";
                mnemonic = "O";
                key = "obl";
            }
        }
        : column {
            fixed_width = true;
            : text {
                label = "  ";
            }
            : popup_list {
                label = "Justify:";
                mnemonic = "J";
                key = "popup_just";
                edit_width = 13;
            }
            : popup_list {
                label = "Style:";
                mnemonic = "e";
                key = "style";
                edit_width = 13;
            }
            : column {
                children_fixed_width = true;
                : toggle {
                    label = "Upside Down";
                    mnemonic = "U";
                    key = "upsd";
                }
                : toggle {
                    label = "Backward";
                    mnemonic = "B";
                    key = "bkwd";
                }
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}


ddmtext : dialog {
  label = "Modify MText";
  common_fields;
  : row {
    : boxed_column {
      label = "Insertion Point";
      fixed_width = true;
      pick_point_1_button;
      x1_box;
      y1_box;
      z1_box;
    }
    : column {
      : row {
        : edit_box {
          label = "Contents:";
          mnemonic = "n";
          width = 40;
          edit_limit = 250;
          key = "t_string";
        }
        : button {
          label = "Full editor...";
          mnemonic = "F";
          key = "MTextEdit";
        }
      }
      : row {
        :column {
          : popup_list {
            label = "Style:";
            mnemonic = "e";
            edit_width = 20;
            key = "style";
          }
          : popup_list {
            label = "Justify:";
            mnemonic = "J";
            edit_width = 20;
            key = "MTextJustify";
          }
          : popup_list {
            label = "Direction:";
            mnemonic = "D";
            edit_width = 20;
            key = "MTextDirection";
          }
        }
        :column {
          : number_box {
            label = "Width:";
            mnemonic = "W";
            key = "MTextWidth";
          }
          : number_box {
            label = "Text Height:";
            mnemonic = "H";
            key = "hght";
          }
          : number_box {
            label = "Rotation:";
            mnemonic = "R";
            key = "rot";
          }
        }
      }
    }
  }
  spacer;
  ok_cancel_help_errtile;
}


ddattdef : dialog {
    label = "Modify Attribute Definition";
    common_fields;
    : row {
        children_fixed_width = true;
        : edit_box {
            label = "Tag:";
            mnemonic = "a";
            key = "tag";
            edit_width = 12;
            edit_limit = 2048;
        }
        : edit_box {
            label = "Prompt:";
            mnemonic = "r";
            key = "prompt";
            edit_width = 12;
            edit_limit = 2048;
        }
        : edit_box {
            label = "Default:";
            mnemonic = "D";
            key = "t_string";
            edit_width = 12;
            edit_limit = 2048;
        }
    }
    spacer;
    : row {
        fixed_width = true;
        : boxed_column {
            label = "Origin";
            fixed_width = true;
            pick_point_1_button;
            x1_box;
            y1_box;
            z1_box;
        }
        : column {
            fixed_width = true;
            : text {
                label = "  ";
            }
            : number_box {
                label = "Height: ";
                mnemonic = "g";
                key = "hght";
            }
            : number_box {
                label = "Rotation: ";
                mnemonic = "R";
                key = "rot";
            }
            : number_box {
                label = "Width Factor: ";
                mnemonic = "W";
                key = "wid";
            }
            : number_box {
                label = "Obliquing: ";
                mnemonic = "O";
                key = "obl";
            }
        }
        : column {
            fixed_width = true;
            : text {
                label = "  ";
            }
            : row {
                : popup_list {
                    label = "Justify:";
                    mnemonic = "J";
                    key = "popup_just";
                    edit_width = 13;
                }
            }
            : row {
                : popup_list {
                    label = "Style:";
                    mnemonic = "e";
                    key = "style";
                    edit_width = 13;
                }
            }
            : row {
                : column {
                   children_fixed_width = true;
                   : toggle {
                      label = "&Upside Down";
                      key = "upsd";
                   }
                   : toggle {
                       label = "&Backward";
                       key = "bkwd";
                   }
                   : toggle {
                       label = "In&visible";
                       key = "inv";
                   }
                }
                : column {
                   children_fixed_width = true;
                   : toggle {
                       label = "Co&nstant";
                       key = "con";
                   }
                   : toggle {
                       label = "Veri&fy";
                       key = "ver";
                   }
                   : toggle {
                       label = "Prese&t";
                       key = "pre";
                   }
                }
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddpline : dialog {
    label = "Modify Polyline";
    common_fields;
    : row {
        fixed_width = true;
        : concatenation {
            : text_part {
                label = "Polyline Type: ";
            }
            : text_part {
                key = "ptype";
                width = 19;
            }
        }
    }
    spacer;
    : row {
        : boxed_column {
            label = "Vertex Listing";
            fixed_width = true;
            : row {
                : column {
                    vertical_margin = wide;
                    : concatenation {
                        : text_part {
                            label = "Vertex:";
                        }
                        : text_part {
                            key = "ctr";
                            width = 4;
                        }
                    }
                }
                : column {
                    : button {
                        label = "Next";
                        mnemonic = "N";
                        key = "next_v";
                    }
                }
            }
            : concatenation {
                : text_part {
                  label = "X: ";
                }
                : text_part {
                  key = "xtext";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Y: ";
                }
                : text_part {
                  key = "ytext";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Z: ";
                }
                : text_part {
                  key = "ztext";
                  width = 10;
                }
            }
        }
        spacer;
        : boxed_radio_column {
            label = "Fit/Smooth";
            key = "f-s";
            : radio_button {
                label = "None";
                mnemonic = "o";
                key = "none";
            }
            : radio_button {
                label = "Quadratic";
                mnemonic = "Q";
                key = "quad";
            }
            : radio_button {
                label = "Cubic";
                mnemonic = "b";
                key = "cubic";
            }
            : radio_button {
                label = "Bezier";
                mnemonic = "B";
                key = "bezier";
            }
            : radio_button {
                label = "Curve Fit";
                mnemonic = "r";
                key = "fit";
            }
        }
        spacer;
        : boxed_column {
            fixed_width = true;
            label = "Mesh";
            key = "mesh";
            : row {
                : concatenation {
                    : text_part {
                         label = "M:";
                    }
                    : text_part {
                         key = "m";
                         width = 3;
                    }
                }
                spacer_1;
                : toggle {
                     label = "Closed";
                     mnemonic = "e";
                     key = "closedm";
                }
            }
            : row {
                : concatenation {
                    : text_part {
                        label = "N:";
                    }
                    : text_part {
                        key = "n";
                        width = 3;
                    }
                }
                spacer_1;
                : toggle {
                     label = "Closed";
                     mnemonic = "e";
                     key = "closedn";
                }
            }
            : edit_box {
                fixed_width = true;
                label = "U:";
                mnemonic = "U";
                key = "u";
                edit_width = 3;
            }
            : edit_box {
                fixed_width = true;
                label = "V:";
                mnemonic = "V";
                key = "v";
                edit_width = 3;
            }
        }
        spacer;
        : boxed_column {
            fixed_width = true;
            label = "Polyline";
            key = "pline";
            : column {
               fixed_height = true;
               : toggle {
                   label = "Closed";
                   mnemonic = "d";
                   key = "closed";
               }
               : toggle {
                   label = "LT Gen";
                   mnemonic = "G";
                   key = "ltgen";
               }
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddvport : dialog {
    label = "Modify Viewport";
    common_fields;
    : row {
        : boxed_column {
            label = "View Center";
            fixed_width = true;
            : concatenation {
                : text_part {
                  label = "X: ";
                }
                : text_part {
                  key = "xtext";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Y: ";
                }
                : text_part {
                  key = "ytext";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Z: ";
                }
                : text_part {
                  key = "ztext";
                  width = 10;
                }
            }
        }
        spacer_0;
        : column {
            fixed_width = true;
            : text {
               label = "   ";
            }
            : concatenation {
                : text_part {
                  label = "Vport ID:";
                  width = 10;
                }
                : text_part {
                  key = "vpid";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Width:";
                  width = 10;
                }
                : text_part {
                  key = "wid";
                  width = 10;
                }
            }
            : concatenation {
                : text_part {
                  label = "Height:";
                  width = 10;
                }
                : text_part {
                  key = "hght";
                  width = 10;
                }
            }
        }
        spacer_0;
        : column {
            alignment = top;
            fixed_width = true;
            fixed_height = true;
            : text {
               label = "   ";
            }
            : concatenation {
                : text_part {
                  label = "Status: ";
                  width = 8;
                }
                : text_part {
                  key = "on-off";
                  width = 15;
                }
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}

ddimen : dialog {
  label = "Modify Dimension";
  common_fields;
  : row {
    : column {
      fixed_height = true;
      fixed_width = true;
      alignment = top;
      : edit_box {
        label = "Contents:";
        mnemonic = "n";
        key = "t_string";
        edit_width = 25;
        edit_limit = 250;
      }
      spacer;
      : popup_list {
        label = "Style: ";
        mnemonic = "e";
        key = "mod_style";
        edit_width = 25;
        list = "";
      }
    }
    spacer;
    : button {
      alignment = top;
      label = "Full editor...";
      mnemonic = "d";
      key = "mod_text";
    }
    : spacer { width = 2; }
    dimstyle_buttons;
  }
  spacer;
  ok_cancel_help_errtile;
}


ddleader : dialog {
    label = "Modify Leader";
    common_fields;
    :row{
        fixed_width = true;
        alignment = centered;
        dimedit_column;
        :spacer { width = 2; }
        dimstyle_buttons;
        :column{
            : radio_column {
                label = "Type";
                key = "s-s";
                : radio_button {
                    label = "St&raight";
                    key = "straight";
                }
                : radio_button {
                    label = "S&pline";
                    key = "spline";
                }
            }
            : toggle {
                label = "&Arrow";
                height = 2;
                key = "arrow";
                fixed_width = true;
            }
        }
    }
    spacer;
    ok_cancel_help_errtile;
}//===end of Modify Leader

ddtolerance : dialog {
    label = "Modify Tolerance";
    common_fields;
    :row{
        fixed_width = true;
        alignment = centered;
        dimedit_column;
        :spacer { width = 2; }
        dimstyle_buttons;
    }
    spacer;
    ok_cancel_help_errtile;
}
//===end of Modify Tolerance

setltype : dialog {
    label = "Select Linetype";
    image_block;
    : list_box {
        height = 12;
		key = "list_lt";
        allow_accept = true;
    }
    : edit_box {
        key = "edit_lt";
        allow_accept = false;
        label = "Linetype:";
        mnemonic = "L";
		edit_width = 32;
	    edit_limit = 217;
    }
    ok_cancel_err;
}

setlayer : dialog {
    subassembly = 0;
    label = "Select Layer";
    initial_focus = "listbox";
    : concatenation {
        children_fixed_width = true;
        key = "clayer";
        : text_part {
            label = "Current Layer: ";
            width = 15;
        }
        : text_part {
            key = "cur_layer";
            width = 35;
        }
    }
    : list_box {
        height = 12;
        key = "list_lay";
        allow_accept = true;
    }
    : row {
        key = "controls";
        : column {
            key = "lname";
            fixed_width = true;
            : edit_box {
                label = "Set Layer Name:";
                mnemonic = "S";
                key = "edit_lay";
                edit_width = 32;
                edit_limit = 217;
                allow_accept = true;
            }
        }
    }
    ok_cancel_err;
}
