dcl_settings : default_dcl_settings { audit_level = 0; }
ddedits : dialog {
  label = "EDIT BY HAN SANG GEUN ";
  initial_focus="edt_txt";
  aspect_ratio=0;
  :text {
      label=""; 
      key="dsp_ver1";
  }
  :text {
      label=""; 
      key="dsp_ver2";
  }
  spacer_1;
  :row {
    :text { label="Text : ";    mnemonic="T";}
    :edit_box{
        key="edt_txt";
        edit_width=70;
        edit_limit=256;
        allow_accept=true;
    }
  }
  :row {
    
    :boxed_column {
       :text {label="사용한것 + 한글"; }
       :list_box{
          key="chg_txt";
          edit_width=15;
          height = 31 ;
       }
    }
  }
  :row {
    spacer_1;
    :button {
       label="Cancel";
       key="cancel";
       fixed_width=true;
       width=8;
       alignment=centered;
       is_cancel=true;
       mnemonic="C";
    }
    :button {
       label="Default";
       key="default";
       fixed_width=true;
       width=8;
       alignment=centered;
       mnemonic="D";
    }
    :button {
       label="chg. All";
       key="chg_all";
       fixed_width=true;
       width=8;
       alignment=centered;
       mnemonic="A";
    }
    :button {
       label="Select";
       key="chg_sel";
       fixed_width=true;
       width=8;
       alignment=centered;
       mnemonic="S";
    }
    ok_button;
    spacer_1;
  }
}
