// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : CooleyTukeyFFT_14
// Git hash  : 0ddbc7657b05706f03c0a8a363d7dc0d9d001858



module CooleyTukeyFFT_14 (
  input      [16:0]   io_input_0,
  input      [16:0]   io_input_1,
  input      [16:0]   io_input_2,
  input      [16:0]   io_input_3,
  input      [16:0]   io_input_4,
  input      [16:0]   io_input_5,
  input      [16:0]   io_input_6,
  input      [16:0]   io_input_7,
  input      [16:0]   io_input_8,
  input      [16:0]   io_input_9,
  input      [16:0]   io_input_10,
  input      [16:0]   io_input_11,
  input      [16:0]   io_input_12,
  input      [16:0]   io_input_13,
  input      [16:0]   io_input_14,
  input      [16:0]   io_input_15,
  input      [16:0]   io_input_16,
  input      [16:0]   io_input_17,
  input      [16:0]   io_input_18,
  input      [16:0]   io_input_19,
  input      [16:0]   io_input_20,
  input      [16:0]   io_input_21,
  input      [16:0]   io_input_22,
  input      [16:0]   io_input_23,
  input      [16:0]   io_input_24,
  input      [16:0]   io_input_25,
  input      [16:0]   io_input_26,
  input      [16:0]   io_input_27,
  input      [16:0]   io_input_28,
  input      [16:0]   io_input_29,
  input      [16:0]   io_input_30,
  input      [16:0]   io_input_31,
  output     [16:0]   io_output_0,
  output     [16:0]   io_output_1,
  output     [16:0]   io_output_2,
  output     [16:0]   io_output_3,
  output     [16:0]   io_output_4,
  output     [16:0]   io_output_5,
  output     [16:0]   io_output_6,
  output     [16:0]   io_output_7,
  output     [16:0]   io_output_8,
  output     [16:0]   io_output_9,
  output     [16:0]   io_output_10,
  output     [16:0]   io_output_11,
  output     [16:0]   io_output_12,
  output     [16:0]   io_output_13,
  output     [16:0]   io_output_14,
  output     [16:0]   io_output_15,
  output     [16:0]   io_output_16,
  output     [16:0]   io_output_17,
  output     [16:0]   io_output_18,
  output     [16:0]   io_output_19,
  output     [16:0]   io_output_20,
  output     [16:0]   io_output_21,
  output     [16:0]   io_output_22,
  output     [16:0]   io_output_23,
  output     [16:0]   io_output_24,
  output     [16:0]   io_output_25,
  output     [16:0]   io_output_26,
  output     [16:0]   io_output_27,
  output     [16:0]   io_output_28,
  output     [16:0]   io_output_29,
  output     [16:0]   io_output_30,
  output     [16:0]   io_output_31
);
  wire       [16:0]   _zz_49;
  wire       [16:0]   _zz_50;
  wire       [16:0]   _zz_51;
  wire       [16:0]   _zz_52;
  wire       [16:0]   _zz_53;
  wire       [16:0]   _zz_54;
  wire       [16:0]   _zz_55;
  wire       [16:0]   _zz_56;
  wire       [16:0]   _zz_57;
  wire       [16:0]   _zz_58;
  wire       [16:0]   _zz_59;
  wire       [16:0]   _zz_60;
  wire       [16:0]   _zz_61;
  wire       [16:0]   _zz_62;
  wire       [16:0]   _zz_63;
  wire       [16:0]   _zz_64;
  wire       [16:0]   _zz_65;
  wire       [16:0]   _zz_66;
  wire       [16:0]   _zz_67;
  wire       [16:0]   _zz_68;
  wire       [16:0]   _zz_69;
  wire       [16:0]   _zz_70;
  wire       [16:0]   _zz_71;
  wire       [16:0]   _zz_72;
  wire       [16:0]   _zz_73;
  wire       [16:0]   _zz_74;
  wire       [16:0]   _zz_75;
  wire       [16:0]   _zz_76;
  wire       [16:0]   _zz_77;
  wire       [16:0]   _zz_78;
  wire       [16:0]   _zz_79;
  wire       [16:0]   _zz_80;
  wire       [16:0]   winogradDFT_32_io_output_0;
  wire       [16:0]   winogradDFT_32_io_output_1;
  wire       [16:0]   winogradDFT_32_io_output_2;
  wire       [16:0]   winogradDFT_32_io_output_3;
  wire       [16:0]   winogradDFT_33_io_output_0;
  wire       [16:0]   winogradDFT_33_io_output_1;
  wire       [16:0]   winogradDFT_33_io_output_2;
  wire       [16:0]   winogradDFT_33_io_output_3;
  wire       [16:0]   winogradDFT_34_io_output_0;
  wire       [16:0]   winogradDFT_34_io_output_1;
  wire       [16:0]   winogradDFT_34_io_output_2;
  wire       [16:0]   winogradDFT_34_io_output_3;
  wire       [16:0]   winogradDFT_35_io_output_0;
  wire       [16:0]   winogradDFT_35_io_output_1;
  wire       [16:0]   winogradDFT_35_io_output_2;
  wire       [16:0]   winogradDFT_35_io_output_3;
  wire       [16:0]   winogradDFT_36_io_output_0;
  wire       [16:0]   winogradDFT_36_io_output_1;
  wire       [16:0]   winogradDFT_36_io_output_2;
  wire       [16:0]   winogradDFT_36_io_output_3;
  wire       [16:0]   winogradDFT_37_io_output_0;
  wire       [16:0]   winogradDFT_37_io_output_1;
  wire       [16:0]   winogradDFT_37_io_output_2;
  wire       [16:0]   winogradDFT_37_io_output_3;
  wire       [16:0]   winogradDFT_38_io_output_0;
  wire       [16:0]   winogradDFT_38_io_output_1;
  wire       [16:0]   winogradDFT_38_io_output_2;
  wire       [16:0]   winogradDFT_38_io_output_3;
  wire       [16:0]   winogradDFT_39_io_output_0;
  wire       [16:0]   winogradDFT_39_io_output_1;
  wire       [16:0]   winogradDFT_39_io_output_2;
  wire       [16:0]   winogradDFT_39_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_0;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_1;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_2;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_4;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_5;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_6;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_7;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_8;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_9;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_10;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_11;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_12;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_13;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_14;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_15;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_0;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_1;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_2;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_4;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_5;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_6;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_7;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_8;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_9;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_10;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_11;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_12;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_13;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_14;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_15;
  wire       [16:0]   _zz_81;
  wire       [25:0]   _zz_82;
  wire       [33:0]   _zz_83;
  wire       [33:0]   _zz_84;
  wire       [16:0]   _zz_85;
  wire       [25:0]   _zz_86;
  wire       [33:0]   _zz_87;
  wire       [33:0]   _zz_88;
  wire       [16:0]   _zz_89;
  wire       [16:0]   _zz_90;
  wire       [25:0]   _zz_91;
  wire       [33:0]   _zz_92;
  wire       [33:0]   _zz_93;
  wire       [16:0]   _zz_94;
  wire       [25:0]   _zz_95;
  wire       [33:0]   _zz_96;
  wire       [33:0]   _zz_97;
  wire       [16:0]   _zz_98;
  wire       [16:0]   _zz_99;
  wire       [25:0]   _zz_100;
  wire       [33:0]   _zz_101;
  wire       [33:0]   _zz_102;
  wire       [16:0]   _zz_103;
  wire       [25:0]   _zz_104;
  wire       [33:0]   _zz_105;
  wire       [33:0]   _zz_106;
  wire       [16:0]   _zz_107;
  wire       [16:0]   _zz_108;
  wire       [25:0]   _zz_109;
  wire       [33:0]   _zz_110;
  wire       [33:0]   _zz_111;
  wire       [16:0]   _zz_112;
  wire       [25:0]   _zz_113;
  wire       [33:0]   _zz_114;
  wire       [33:0]   _zz_115;
  wire       [16:0]   _zz_116;
  wire       [16:0]   _zz_117;
  wire       [25:0]   _zz_118;
  wire       [33:0]   _zz_119;
  wire       [33:0]   _zz_120;
  wire       [16:0]   _zz_121;
  wire       [25:0]   _zz_122;
  wire       [33:0]   _zz_123;
  wire       [33:0]   _zz_124;
  wire       [16:0]   _zz_125;
  wire       [16:0]   _zz_126;
  wire       [25:0]   _zz_127;
  wire       [33:0]   _zz_128;
  wire       [33:0]   _zz_129;
  wire       [16:0]   _zz_130;
  wire       [25:0]   _zz_131;
  wire       [33:0]   _zz_132;
  wire       [33:0]   _zz_133;
  wire       [16:0]   _zz_134;
  wire       [16:0]   _zz_135;
  wire       [25:0]   _zz_136;
  wire       [33:0]   _zz_137;
  wire       [33:0]   _zz_138;
  wire       [16:0]   _zz_139;
  wire       [25:0]   _zz_140;
  wire       [33:0]   _zz_141;
  wire       [33:0]   _zz_142;
  wire       [16:0]   _zz_143;
  wire       [16:0]   _zz_144;
  wire       [25:0]   _zz_145;
  wire       [33:0]   _zz_146;
  wire       [33:0]   _zz_147;
  wire       [16:0]   _zz_148;
  wire       [25:0]   _zz_149;
  wire       [33:0]   _zz_150;
  wire       [33:0]   _zz_151;
  wire       [16:0]   _zz_152;
  wire       [16:0]   _zz_153;
  wire       [25:0]   _zz_154;
  wire       [33:0]   _zz_155;
  wire       [33:0]   _zz_156;
  wire       [16:0]   _zz_157;
  wire       [25:0]   _zz_158;
  wire       [33:0]   _zz_159;
  wire       [33:0]   _zz_160;
  wire       [16:0]   _zz_161;
  wire       [16:0]   _zz_162;
  wire       [25:0]   _zz_163;
  wire       [33:0]   _zz_164;
  wire       [33:0]   _zz_165;
  wire       [16:0]   _zz_166;
  wire       [25:0]   _zz_167;
  wire       [33:0]   _zz_168;
  wire       [33:0]   _zz_169;
  wire       [16:0]   _zz_170;
  wire       [16:0]   _zz_171;
  wire       [25:0]   _zz_172;
  wire       [33:0]   _zz_173;
  wire       [33:0]   _zz_174;
  wire       [16:0]   _zz_175;
  wire       [25:0]   _zz_176;
  wire       [33:0]   _zz_177;
  wire       [33:0]   _zz_178;
  wire       [16:0]   _zz_179;
  wire       [16:0]   _zz_180;
  wire       [25:0]   _zz_181;
  wire       [33:0]   _zz_182;
  wire       [33:0]   _zz_183;
  wire       [16:0]   _zz_184;
  wire       [25:0]   _zz_185;
  wire       [33:0]   _zz_186;
  wire       [33:0]   _zz_187;
  wire       [16:0]   _zz_188;
  wire       [16:0]   _zz_189;
  wire       [25:0]   _zz_190;
  wire       [33:0]   _zz_191;
  wire       [33:0]   _zz_192;
  wire       [16:0]   _zz_193;
  wire       [25:0]   _zz_194;
  wire       [33:0]   _zz_195;
  wire       [33:0]   _zz_196;
  wire       [16:0]   _zz_197;
  wire       [16:0]   _zz_198;
  wire       [25:0]   _zz_199;
  wire       [33:0]   _zz_200;
  wire       [33:0]   _zz_201;
  wire       [16:0]   _zz_202;
  wire       [25:0]   _zz_203;
  wire       [33:0]   _zz_204;
  wire       [33:0]   _zz_205;
  wire       [16:0]   _zz_206;
  wire       [16:0]   _zz_207;
  wire       [25:0]   _zz_208;
  wire       [33:0]   _zz_209;
  wire       [33:0]   _zz_210;
  wire       [16:0]   _zz_211;
  wire       [25:0]   _zz_212;
  wire       [33:0]   _zz_213;
  wire       [33:0]   _zz_214;
  wire       [16:0]   _zz_215;
  wire       [16:0]   _zz_216;
  wire       [25:0]   _zz_217;
  wire       [33:0]   _zz_218;
  wire       [33:0]   _zz_219;
  wire       [16:0]   _zz_220;
  wire       [25:0]   _zz_221;
  wire       [33:0]   _zz_222;
  wire       [33:0]   _zz_223;
  wire       [16:0]   _zz_224;
  wire       [16:0]   _zz_1;
  wire       [16:0]   _zz_2;
  wire       [33:0]   _zz_3;
  wire       [16:0]   _zz_4;
  wire       [16:0]   _zz_5;
  wire       [33:0]   _zz_6;
  wire       [16:0]   _zz_7;
  wire       [16:0]   _zz_8;
  wire       [33:0]   _zz_9;
  wire       [16:0]   _zz_10;
  wire       [16:0]   _zz_11;
  wire       [33:0]   _zz_12;
  wire       [16:0]   _zz_13;
  wire       [16:0]   _zz_14;
  wire       [33:0]   _zz_15;
  wire       [16:0]   _zz_16;
  wire       [16:0]   _zz_17;
  wire       [33:0]   _zz_18;
  wire       [16:0]   _zz_19;
  wire       [16:0]   _zz_20;
  wire       [33:0]   _zz_21;
  wire       [16:0]   _zz_22;
  wire       [16:0]   _zz_23;
  wire       [33:0]   _zz_24;
  wire       [16:0]   _zz_25;
  wire       [16:0]   _zz_26;
  wire       [33:0]   _zz_27;
  wire       [16:0]   _zz_28;
  wire       [16:0]   _zz_29;
  wire       [33:0]   _zz_30;
  wire       [16:0]   _zz_31;
  wire       [16:0]   _zz_32;
  wire       [33:0]   _zz_33;
  wire       [16:0]   _zz_34;
  wire       [16:0]   _zz_35;
  wire       [33:0]   _zz_36;
  wire       [16:0]   _zz_37;
  wire       [16:0]   _zz_38;
  wire       [33:0]   _zz_39;
  wire       [16:0]   _zz_40;
  wire       [16:0]   _zz_41;
  wire       [33:0]   _zz_42;
  wire       [16:0]   _zz_43;
  wire       [16:0]   _zz_44;
  wire       [33:0]   _zz_45;
  wire       [16:0]   _zz_46;
  wire       [16:0]   _zz_47;
  wire       [33:0]   _zz_48;

  assign _zz_81 = ($signed(cooleyTukeyFFT_15_io_output_0) - $signed(cooleyTukeyFFT_15_io_output_1));
  assign _zz_82 = (_zz_83 >>> 8);
  assign _zz_83 = ($signed(_zz_84) + $signed(_zz_3));
  assign _zz_84 = ($signed(_zz_85) * $signed(cooleyTukeyFFT_15_io_output_1));
  assign _zz_85 = ($signed(_zz_1) - $signed(_zz_2));
  assign _zz_86 = (_zz_87 >>> 8);
  assign _zz_87 = ($signed(_zz_88) - $signed(_zz_3));
  assign _zz_88 = ($signed(_zz_89) * $signed(cooleyTukeyFFT_15_io_output_0));
  assign _zz_89 = ($signed(_zz_1) + $signed(_zz_2));
  assign _zz_90 = ($signed(cooleyTukeyFFT_16_io_output_0) - $signed(cooleyTukeyFFT_16_io_output_1));
  assign _zz_91 = (_zz_92 >>> 8);
  assign _zz_92 = ($signed(_zz_93) + $signed(_zz_6));
  assign _zz_93 = ($signed(_zz_94) * $signed(cooleyTukeyFFT_16_io_output_1));
  assign _zz_94 = ($signed(_zz_4) - $signed(_zz_5));
  assign _zz_95 = (_zz_96 >>> 8);
  assign _zz_96 = ($signed(_zz_97) - $signed(_zz_6));
  assign _zz_97 = ($signed(_zz_98) * $signed(cooleyTukeyFFT_16_io_output_0));
  assign _zz_98 = ($signed(_zz_4) + $signed(_zz_5));
  assign _zz_99 = ($signed(cooleyTukeyFFT_15_io_output_2) - $signed(cooleyTukeyFFT_15_io_output_3));
  assign _zz_100 = (_zz_101 >>> 8);
  assign _zz_101 = ($signed(_zz_102) + $signed(_zz_9));
  assign _zz_102 = ($signed(_zz_103) * $signed(cooleyTukeyFFT_15_io_output_3));
  assign _zz_103 = ($signed(_zz_7) - $signed(_zz_8));
  assign _zz_104 = (_zz_105 >>> 8);
  assign _zz_105 = ($signed(_zz_106) - $signed(_zz_9));
  assign _zz_106 = ($signed(_zz_107) * $signed(cooleyTukeyFFT_15_io_output_2));
  assign _zz_107 = ($signed(_zz_7) + $signed(_zz_8));
  assign _zz_108 = ($signed(cooleyTukeyFFT_16_io_output_2) - $signed(cooleyTukeyFFT_16_io_output_3));
  assign _zz_109 = (_zz_110 >>> 8);
  assign _zz_110 = ($signed(_zz_111) + $signed(_zz_12));
  assign _zz_111 = ($signed(_zz_112) * $signed(cooleyTukeyFFT_16_io_output_3));
  assign _zz_112 = ($signed(_zz_10) - $signed(_zz_11));
  assign _zz_113 = (_zz_114 >>> 8);
  assign _zz_114 = ($signed(_zz_115) - $signed(_zz_12));
  assign _zz_115 = ($signed(_zz_116) * $signed(cooleyTukeyFFT_16_io_output_2));
  assign _zz_116 = ($signed(_zz_10) + $signed(_zz_11));
  assign _zz_117 = ($signed(cooleyTukeyFFT_15_io_output_4) - $signed(cooleyTukeyFFT_15_io_output_5));
  assign _zz_118 = (_zz_119 >>> 8);
  assign _zz_119 = ($signed(_zz_120) + $signed(_zz_15));
  assign _zz_120 = ($signed(_zz_121) * $signed(cooleyTukeyFFT_15_io_output_5));
  assign _zz_121 = ($signed(_zz_13) - $signed(_zz_14));
  assign _zz_122 = (_zz_123 >>> 8);
  assign _zz_123 = ($signed(_zz_124) - $signed(_zz_15));
  assign _zz_124 = ($signed(_zz_125) * $signed(cooleyTukeyFFT_15_io_output_4));
  assign _zz_125 = ($signed(_zz_13) + $signed(_zz_14));
  assign _zz_126 = ($signed(cooleyTukeyFFT_16_io_output_4) - $signed(cooleyTukeyFFT_16_io_output_5));
  assign _zz_127 = (_zz_128 >>> 8);
  assign _zz_128 = ($signed(_zz_129) + $signed(_zz_18));
  assign _zz_129 = ($signed(_zz_130) * $signed(cooleyTukeyFFT_16_io_output_5));
  assign _zz_130 = ($signed(_zz_16) - $signed(_zz_17));
  assign _zz_131 = (_zz_132 >>> 8);
  assign _zz_132 = ($signed(_zz_133) - $signed(_zz_18));
  assign _zz_133 = ($signed(_zz_134) * $signed(cooleyTukeyFFT_16_io_output_4));
  assign _zz_134 = ($signed(_zz_16) + $signed(_zz_17));
  assign _zz_135 = ($signed(cooleyTukeyFFT_15_io_output_6) - $signed(cooleyTukeyFFT_15_io_output_7));
  assign _zz_136 = (_zz_137 >>> 8);
  assign _zz_137 = ($signed(_zz_138) + $signed(_zz_21));
  assign _zz_138 = ($signed(_zz_139) * $signed(cooleyTukeyFFT_15_io_output_7));
  assign _zz_139 = ($signed(_zz_19) - $signed(_zz_20));
  assign _zz_140 = (_zz_141 >>> 8);
  assign _zz_141 = ($signed(_zz_142) - $signed(_zz_21));
  assign _zz_142 = ($signed(_zz_143) * $signed(cooleyTukeyFFT_15_io_output_6));
  assign _zz_143 = ($signed(_zz_19) + $signed(_zz_20));
  assign _zz_144 = ($signed(cooleyTukeyFFT_16_io_output_6) - $signed(cooleyTukeyFFT_16_io_output_7));
  assign _zz_145 = (_zz_146 >>> 8);
  assign _zz_146 = ($signed(_zz_147) + $signed(_zz_24));
  assign _zz_147 = ($signed(_zz_148) * $signed(cooleyTukeyFFT_16_io_output_7));
  assign _zz_148 = ($signed(_zz_22) - $signed(_zz_23));
  assign _zz_149 = (_zz_150 >>> 8);
  assign _zz_150 = ($signed(_zz_151) - $signed(_zz_24));
  assign _zz_151 = ($signed(_zz_152) * $signed(cooleyTukeyFFT_16_io_output_6));
  assign _zz_152 = ($signed(_zz_22) + $signed(_zz_23));
  assign _zz_153 = ($signed(cooleyTukeyFFT_15_io_output_8) - $signed(cooleyTukeyFFT_15_io_output_9));
  assign _zz_154 = (_zz_155 >>> 8);
  assign _zz_155 = ($signed(_zz_156) + $signed(_zz_27));
  assign _zz_156 = ($signed(_zz_157) * $signed(cooleyTukeyFFT_15_io_output_9));
  assign _zz_157 = ($signed(_zz_25) - $signed(_zz_26));
  assign _zz_158 = (_zz_159 >>> 8);
  assign _zz_159 = ($signed(_zz_160) - $signed(_zz_27));
  assign _zz_160 = ($signed(_zz_161) * $signed(cooleyTukeyFFT_15_io_output_8));
  assign _zz_161 = ($signed(_zz_25) + $signed(_zz_26));
  assign _zz_162 = ($signed(cooleyTukeyFFT_16_io_output_8) - $signed(cooleyTukeyFFT_16_io_output_9));
  assign _zz_163 = (_zz_164 >>> 8);
  assign _zz_164 = ($signed(_zz_165) + $signed(_zz_30));
  assign _zz_165 = ($signed(_zz_166) * $signed(cooleyTukeyFFT_16_io_output_9));
  assign _zz_166 = ($signed(_zz_28) - $signed(_zz_29));
  assign _zz_167 = (_zz_168 >>> 8);
  assign _zz_168 = ($signed(_zz_169) - $signed(_zz_30));
  assign _zz_169 = ($signed(_zz_170) * $signed(cooleyTukeyFFT_16_io_output_8));
  assign _zz_170 = ($signed(_zz_28) + $signed(_zz_29));
  assign _zz_171 = ($signed(cooleyTukeyFFT_15_io_output_10) - $signed(cooleyTukeyFFT_15_io_output_11));
  assign _zz_172 = (_zz_173 >>> 8);
  assign _zz_173 = ($signed(_zz_174) + $signed(_zz_33));
  assign _zz_174 = ($signed(_zz_175) * $signed(cooleyTukeyFFT_15_io_output_11));
  assign _zz_175 = ($signed(_zz_31) - $signed(_zz_32));
  assign _zz_176 = (_zz_177 >>> 8);
  assign _zz_177 = ($signed(_zz_178) - $signed(_zz_33));
  assign _zz_178 = ($signed(_zz_179) * $signed(cooleyTukeyFFT_15_io_output_10));
  assign _zz_179 = ($signed(_zz_31) + $signed(_zz_32));
  assign _zz_180 = ($signed(cooleyTukeyFFT_16_io_output_10) - $signed(cooleyTukeyFFT_16_io_output_11));
  assign _zz_181 = (_zz_182 >>> 8);
  assign _zz_182 = ($signed(_zz_183) + $signed(_zz_36));
  assign _zz_183 = ($signed(_zz_184) * $signed(cooleyTukeyFFT_16_io_output_11));
  assign _zz_184 = ($signed(_zz_34) - $signed(_zz_35));
  assign _zz_185 = (_zz_186 >>> 8);
  assign _zz_186 = ($signed(_zz_187) - $signed(_zz_36));
  assign _zz_187 = ($signed(_zz_188) * $signed(cooleyTukeyFFT_16_io_output_10));
  assign _zz_188 = ($signed(_zz_34) + $signed(_zz_35));
  assign _zz_189 = ($signed(cooleyTukeyFFT_15_io_output_12) - $signed(cooleyTukeyFFT_15_io_output_13));
  assign _zz_190 = (_zz_191 >>> 8);
  assign _zz_191 = ($signed(_zz_192) + $signed(_zz_39));
  assign _zz_192 = ($signed(_zz_193) * $signed(cooleyTukeyFFT_15_io_output_13));
  assign _zz_193 = ($signed(_zz_37) - $signed(_zz_38));
  assign _zz_194 = (_zz_195 >>> 8);
  assign _zz_195 = ($signed(_zz_196) - $signed(_zz_39));
  assign _zz_196 = ($signed(_zz_197) * $signed(cooleyTukeyFFT_15_io_output_12));
  assign _zz_197 = ($signed(_zz_37) + $signed(_zz_38));
  assign _zz_198 = ($signed(cooleyTukeyFFT_16_io_output_12) - $signed(cooleyTukeyFFT_16_io_output_13));
  assign _zz_199 = (_zz_200 >>> 8);
  assign _zz_200 = ($signed(_zz_201) + $signed(_zz_42));
  assign _zz_201 = ($signed(_zz_202) * $signed(cooleyTukeyFFT_16_io_output_13));
  assign _zz_202 = ($signed(_zz_40) - $signed(_zz_41));
  assign _zz_203 = (_zz_204 >>> 8);
  assign _zz_204 = ($signed(_zz_205) - $signed(_zz_42));
  assign _zz_205 = ($signed(_zz_206) * $signed(cooleyTukeyFFT_16_io_output_12));
  assign _zz_206 = ($signed(_zz_40) + $signed(_zz_41));
  assign _zz_207 = ($signed(cooleyTukeyFFT_15_io_output_14) - $signed(cooleyTukeyFFT_15_io_output_15));
  assign _zz_208 = (_zz_209 >>> 8);
  assign _zz_209 = ($signed(_zz_210) + $signed(_zz_45));
  assign _zz_210 = ($signed(_zz_211) * $signed(cooleyTukeyFFT_15_io_output_15));
  assign _zz_211 = ($signed(_zz_43) - $signed(_zz_44));
  assign _zz_212 = (_zz_213 >>> 8);
  assign _zz_213 = ($signed(_zz_214) - $signed(_zz_45));
  assign _zz_214 = ($signed(_zz_215) * $signed(cooleyTukeyFFT_15_io_output_14));
  assign _zz_215 = ($signed(_zz_43) + $signed(_zz_44));
  assign _zz_216 = ($signed(cooleyTukeyFFT_16_io_output_14) - $signed(cooleyTukeyFFT_16_io_output_15));
  assign _zz_217 = (_zz_218 >>> 8);
  assign _zz_218 = ($signed(_zz_219) + $signed(_zz_48));
  assign _zz_219 = ($signed(_zz_220) * $signed(cooleyTukeyFFT_16_io_output_15));
  assign _zz_220 = ($signed(_zz_46) - $signed(_zz_47));
  assign _zz_221 = (_zz_222 >>> 8);
  assign _zz_222 = ($signed(_zz_223) - $signed(_zz_48));
  assign _zz_223 = ($signed(_zz_224) * $signed(cooleyTukeyFFT_16_io_output_14));
  assign _zz_224 = ($signed(_zz_46) + $signed(_zz_47));
  WinogradDFT winogradDFT_32 (
    .io_input_0     (_zz_49[16:0]                      ), //i
    .io_input_1     (_zz_50[16:0]                      ), //i
    .io_input_2     (_zz_51[16:0]                      ), //i
    .io_input_3     (_zz_52[16:0]                      ), //i
    .io_output_0    (winogradDFT_32_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_32_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_32_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_32_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_33 (
    .io_input_0     (_zz_53[16:0]                      ), //i
    .io_input_1     (_zz_54[16:0]                      ), //i
    .io_input_2     (_zz_55[16:0]                      ), //i
    .io_input_3     (_zz_56[16:0]                      ), //i
    .io_output_0    (winogradDFT_33_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_33_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_33_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_33_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_34 (
    .io_input_0     (_zz_57[16:0]                      ), //i
    .io_input_1     (_zz_58[16:0]                      ), //i
    .io_input_2     (_zz_59[16:0]                      ), //i
    .io_input_3     (_zz_60[16:0]                      ), //i
    .io_output_0    (winogradDFT_34_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_34_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_34_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_34_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_35 (
    .io_input_0     (_zz_61[16:0]                      ), //i
    .io_input_1     (_zz_62[16:0]                      ), //i
    .io_input_2     (_zz_63[16:0]                      ), //i
    .io_input_3     (_zz_64[16:0]                      ), //i
    .io_output_0    (winogradDFT_35_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_35_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_35_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_35_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_36 (
    .io_input_0     (_zz_65[16:0]                      ), //i
    .io_input_1     (_zz_66[16:0]                      ), //i
    .io_input_2     (_zz_67[16:0]                      ), //i
    .io_input_3     (_zz_68[16:0]                      ), //i
    .io_output_0    (winogradDFT_36_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_36_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_36_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_36_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_37 (
    .io_input_0     (_zz_69[16:0]                      ), //i
    .io_input_1     (_zz_70[16:0]                      ), //i
    .io_input_2     (_zz_71[16:0]                      ), //i
    .io_input_3     (_zz_72[16:0]                      ), //i
    .io_output_0    (winogradDFT_37_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_37_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_37_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_37_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_38 (
    .io_input_0     (_zz_73[16:0]                      ), //i
    .io_input_1     (_zz_74[16:0]                      ), //i
    .io_input_2     (_zz_75[16:0]                      ), //i
    .io_input_3     (_zz_76[16:0]                      ), //i
    .io_output_0    (winogradDFT_38_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_38_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_38_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_38_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_39 (
    .io_input_0     (_zz_77[16:0]                      ), //i
    .io_input_1     (_zz_78[16:0]                      ), //i
    .io_input_2     (_zz_79[16:0]                      ), //i
    .io_input_3     (_zz_80[16:0]                      ), //i
    .io_output_0    (winogradDFT_39_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_39_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_39_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_39_io_output_3[16:0]  )  //o
  );
  CooleyTukeyFFT_12 cooleyTukeyFFT_15 (
    .io_input_0      (io_input_0[16:0]                      ), //i
    .io_input_1      (io_input_1[16:0]                      ), //i
    .io_input_2      (io_input_2[16:0]                      ), //i
    .io_input_3      (io_input_3[16:0]                      ), //i
    .io_input_4      (io_input_4[16:0]                      ), //i
    .io_input_5      (io_input_5[16:0]                      ), //i
    .io_input_6      (io_input_6[16:0]                      ), //i
    .io_input_7      (io_input_7[16:0]                      ), //i
    .io_input_8      (io_input_8[16:0]                      ), //i
    .io_input_9      (io_input_9[16:0]                      ), //i
    .io_input_10     (io_input_10[16:0]                     ), //i
    .io_input_11     (io_input_11[16:0]                     ), //i
    .io_input_12     (io_input_12[16:0]                     ), //i
    .io_input_13     (io_input_13[16:0]                     ), //i
    .io_input_14     (io_input_14[16:0]                     ), //i
    .io_input_15     (io_input_15[16:0]                     ), //i
    .io_output_0     (cooleyTukeyFFT_15_io_output_0[16:0]   ), //o
    .io_output_1     (cooleyTukeyFFT_15_io_output_1[16:0]   ), //o
    .io_output_2     (cooleyTukeyFFT_15_io_output_2[16:0]   ), //o
    .io_output_3     (cooleyTukeyFFT_15_io_output_3[16:0]   ), //o
    .io_output_4     (cooleyTukeyFFT_15_io_output_4[16:0]   ), //o
    .io_output_5     (cooleyTukeyFFT_15_io_output_5[16:0]   ), //o
    .io_output_6     (cooleyTukeyFFT_15_io_output_6[16:0]   ), //o
    .io_output_7     (cooleyTukeyFFT_15_io_output_7[16:0]   ), //o
    .io_output_8     (cooleyTukeyFFT_15_io_output_8[16:0]   ), //o
    .io_output_9     (cooleyTukeyFFT_15_io_output_9[16:0]   ), //o
    .io_output_10    (cooleyTukeyFFT_15_io_output_10[16:0]  ), //o
    .io_output_11    (cooleyTukeyFFT_15_io_output_11[16:0]  ), //o
    .io_output_12    (cooleyTukeyFFT_15_io_output_12[16:0]  ), //o
    .io_output_13    (cooleyTukeyFFT_15_io_output_13[16:0]  ), //o
    .io_output_14    (cooleyTukeyFFT_15_io_output_14[16:0]  ), //o
    .io_output_15    (cooleyTukeyFFT_15_io_output_15[16:0]  )  //o
  );
  CooleyTukeyFFT_12 cooleyTukeyFFT_16 (
    .io_input_0      (io_input_16[16:0]                     ), //i
    .io_input_1      (io_input_17[16:0]                     ), //i
    .io_input_2      (io_input_18[16:0]                     ), //i
    .io_input_3      (io_input_19[16:0]                     ), //i
    .io_input_4      (io_input_20[16:0]                     ), //i
    .io_input_5      (io_input_21[16:0]                     ), //i
    .io_input_6      (io_input_22[16:0]                     ), //i
    .io_input_7      (io_input_23[16:0]                     ), //i
    .io_input_8      (io_input_24[16:0]                     ), //i
    .io_input_9      (io_input_25[16:0]                     ), //i
    .io_input_10     (io_input_26[16:0]                     ), //i
    .io_input_11     (io_input_27[16:0]                     ), //i
    .io_input_12     (io_input_28[16:0]                     ), //i
    .io_input_13     (io_input_29[16:0]                     ), //i
    .io_input_14     (io_input_30[16:0]                     ), //i
    .io_input_15     (io_input_31[16:0]                     ), //i
    .io_output_0     (cooleyTukeyFFT_16_io_output_0[16:0]   ), //o
    .io_output_1     (cooleyTukeyFFT_16_io_output_1[16:0]   ), //o
    .io_output_2     (cooleyTukeyFFT_16_io_output_2[16:0]   ), //o
    .io_output_3     (cooleyTukeyFFT_16_io_output_3[16:0]   ), //o
    .io_output_4     (cooleyTukeyFFT_16_io_output_4[16:0]   ), //o
    .io_output_5     (cooleyTukeyFFT_16_io_output_5[16:0]   ), //o
    .io_output_6     (cooleyTukeyFFT_16_io_output_6[16:0]   ), //o
    .io_output_7     (cooleyTukeyFFT_16_io_output_7[16:0]   ), //o
    .io_output_8     (cooleyTukeyFFT_16_io_output_8[16:0]   ), //o
    .io_output_9     (cooleyTukeyFFT_16_io_output_9[16:0]   ), //o
    .io_output_10    (cooleyTukeyFFT_16_io_output_10[16:0]  ), //o
    .io_output_11    (cooleyTukeyFFT_16_io_output_11[16:0]  ), //o
    .io_output_12    (cooleyTukeyFFT_16_io_output_12[16:0]  ), //o
    .io_output_13    (cooleyTukeyFFT_16_io_output_13[16:0]  ), //o
    .io_output_14    (cooleyTukeyFFT_16_io_output_14[16:0]  ), //o
    .io_output_15    (cooleyTukeyFFT_16_io_output_15[16:0]  )  //o
  );
  assign _zz_1 = 17'h00100;
  assign _zz_2 = 17'h0;
  assign _zz_3 = ($signed(_zz_1) * $signed(_zz_81));
  assign _zz_49 = _zz_82[16:0];
  assign _zz_50 = _zz_86[16:0];
  assign _zz_4 = 17'h00100;
  assign _zz_5 = 17'h0;
  assign _zz_6 = ($signed(_zz_4) * $signed(_zz_90));
  assign _zz_51 = _zz_91[16:0];
  assign _zz_52 = _zz_95[16:0];
  assign _zz_7 = 17'h00100;
  assign _zz_8 = 17'h0;
  assign _zz_9 = ($signed(_zz_7) * $signed(_zz_99));
  assign _zz_53 = _zz_100[16:0];
  assign _zz_54 = _zz_104[16:0];
  assign _zz_10 = 17'h000ec;
  assign _zz_11 = 17'h1ff9f;
  assign _zz_12 = ($signed(_zz_10) * $signed(_zz_108));
  assign _zz_55 = _zz_109[16:0];
  assign _zz_56 = _zz_113[16:0];
  assign _zz_13 = 17'h00100;
  assign _zz_14 = 17'h0;
  assign _zz_15 = ($signed(_zz_13) * $signed(_zz_117));
  assign _zz_57 = _zz_118[16:0];
  assign _zz_58 = _zz_122[16:0];
  assign _zz_16 = 17'h000b5;
  assign _zz_17 = 17'h1ff4b;
  assign _zz_18 = ($signed(_zz_16) * $signed(_zz_126));
  assign _zz_59 = _zz_127[16:0];
  assign _zz_60 = _zz_131[16:0];
  assign _zz_19 = 17'h00100;
  assign _zz_20 = 17'h0;
  assign _zz_21 = ($signed(_zz_19) * $signed(_zz_135));
  assign _zz_61 = _zz_136[16:0];
  assign _zz_62 = _zz_140[16:0];
  assign _zz_22 = 17'h00061;
  assign _zz_23 = 17'h1ff14;
  assign _zz_24 = ($signed(_zz_22) * $signed(_zz_144));
  assign _zz_63 = _zz_145[16:0];
  assign _zz_64 = _zz_149[16:0];
  assign _zz_25 = 17'h00100;
  assign _zz_26 = 17'h0;
  assign _zz_27 = ($signed(_zz_25) * $signed(_zz_153));
  assign _zz_65 = _zz_154[16:0];
  assign _zz_66 = _zz_158[16:0];
  assign _zz_28 = 17'h0;
  assign _zz_29 = 17'h1ff00;
  assign _zz_30 = ($signed(_zz_28) * $signed(_zz_162));
  assign _zz_67 = _zz_163[16:0];
  assign _zz_68 = _zz_167[16:0];
  assign _zz_31 = 17'h00100;
  assign _zz_32 = 17'h0;
  assign _zz_33 = ($signed(_zz_31) * $signed(_zz_171));
  assign _zz_69 = _zz_172[16:0];
  assign _zz_70 = _zz_176[16:0];
  assign _zz_34 = 17'h1ff9f;
  assign _zz_35 = 17'h1ff14;
  assign _zz_36 = ($signed(_zz_34) * $signed(_zz_180));
  assign _zz_71 = _zz_181[16:0];
  assign _zz_72 = _zz_185[16:0];
  assign _zz_37 = 17'h00100;
  assign _zz_38 = 17'h0;
  assign _zz_39 = ($signed(_zz_37) * $signed(_zz_189));
  assign _zz_73 = _zz_190[16:0];
  assign _zz_74 = _zz_194[16:0];
  assign _zz_40 = 17'h1ff4b;
  assign _zz_41 = 17'h1ff4b;
  assign _zz_42 = ($signed(_zz_40) * $signed(_zz_198));
  assign _zz_75 = _zz_199[16:0];
  assign _zz_76 = _zz_203[16:0];
  assign _zz_43 = 17'h00100;
  assign _zz_44 = 17'h0;
  assign _zz_45 = ($signed(_zz_43) * $signed(_zz_207));
  assign _zz_77 = _zz_208[16:0];
  assign _zz_78 = _zz_212[16:0];
  assign _zz_46 = 17'h1ff14;
  assign _zz_47 = 17'h1ff9f;
  assign _zz_48 = ($signed(_zz_46) * $signed(_zz_216));
  assign _zz_79 = _zz_217[16:0];
  assign _zz_80 = _zz_221[16:0];
  assign io_output_0 = winogradDFT_32_io_output_0;
  assign io_output_1 = winogradDFT_32_io_output_1;
  assign io_output_2 = winogradDFT_32_io_output_2;
  assign io_output_3 = winogradDFT_32_io_output_3;
  assign io_output_4 = winogradDFT_33_io_output_0;
  assign io_output_5 = winogradDFT_33_io_output_1;
  assign io_output_6 = winogradDFT_33_io_output_2;
  assign io_output_7 = winogradDFT_33_io_output_3;
  assign io_output_8 = winogradDFT_34_io_output_0;
  assign io_output_9 = winogradDFT_34_io_output_1;
  assign io_output_10 = winogradDFT_34_io_output_2;
  assign io_output_11 = winogradDFT_34_io_output_3;
  assign io_output_12 = winogradDFT_35_io_output_0;
  assign io_output_13 = winogradDFT_35_io_output_1;
  assign io_output_14 = winogradDFT_35_io_output_2;
  assign io_output_15 = winogradDFT_35_io_output_3;
  assign io_output_16 = winogradDFT_36_io_output_0;
  assign io_output_17 = winogradDFT_36_io_output_1;
  assign io_output_18 = winogradDFT_36_io_output_2;
  assign io_output_19 = winogradDFT_36_io_output_3;
  assign io_output_20 = winogradDFT_37_io_output_0;
  assign io_output_21 = winogradDFT_37_io_output_1;
  assign io_output_22 = winogradDFT_37_io_output_2;
  assign io_output_23 = winogradDFT_37_io_output_3;
  assign io_output_24 = winogradDFT_38_io_output_0;
  assign io_output_25 = winogradDFT_38_io_output_1;
  assign io_output_26 = winogradDFT_38_io_output_2;
  assign io_output_27 = winogradDFT_38_io_output_3;
  assign io_output_28 = winogradDFT_39_io_output_0;
  assign io_output_29 = winogradDFT_39_io_output_1;
  assign io_output_30 = winogradDFT_39_io_output_2;
  assign io_output_31 = winogradDFT_39_io_output_3;

endmodule

//CooleyTukeyFFT_12 replaced by CooleyTukeyFFT_12

module CooleyTukeyFFT_12 (
  input      [16:0]   io_input_0,
  input      [16:0]   io_input_1,
  input      [16:0]   io_input_2,
  input      [16:0]   io_input_3,
  input      [16:0]   io_input_4,
  input      [16:0]   io_input_5,
  input      [16:0]   io_input_6,
  input      [16:0]   io_input_7,
  input      [16:0]   io_input_8,
  input      [16:0]   io_input_9,
  input      [16:0]   io_input_10,
  input      [16:0]   io_input_11,
  input      [16:0]   io_input_12,
  input      [16:0]   io_input_13,
  input      [16:0]   io_input_14,
  input      [16:0]   io_input_15,
  output     [16:0]   io_output_0,
  output     [16:0]   io_output_1,
  output     [16:0]   io_output_2,
  output     [16:0]   io_output_3,
  output     [16:0]   io_output_4,
  output     [16:0]   io_output_5,
  output     [16:0]   io_output_6,
  output     [16:0]   io_output_7,
  output     [16:0]   io_output_8,
  output     [16:0]   io_output_9,
  output     [16:0]   io_output_10,
  output     [16:0]   io_output_11,
  output     [16:0]   io_output_12,
  output     [16:0]   io_output_13,
  output     [16:0]   io_output_14,
  output     [16:0]   io_output_15
);
  wire       [16:0]   _zz_25;
  wire       [16:0]   _zz_26;
  wire       [16:0]   _zz_27;
  wire       [16:0]   _zz_28;
  wire       [16:0]   _zz_29;
  wire       [16:0]   _zz_30;
  wire       [16:0]   _zz_31;
  wire       [16:0]   _zz_32;
  wire       [16:0]   _zz_33;
  wire       [16:0]   _zz_34;
  wire       [16:0]   _zz_35;
  wire       [16:0]   _zz_36;
  wire       [16:0]   _zz_37;
  wire       [16:0]   _zz_38;
  wire       [16:0]   _zz_39;
  wire       [16:0]   _zz_40;
  wire       [16:0]   winogradDFT_32_io_output_0;
  wire       [16:0]   winogradDFT_32_io_output_1;
  wire       [16:0]   winogradDFT_32_io_output_2;
  wire       [16:0]   winogradDFT_32_io_output_3;
  wire       [16:0]   winogradDFT_33_io_output_0;
  wire       [16:0]   winogradDFT_33_io_output_1;
  wire       [16:0]   winogradDFT_33_io_output_2;
  wire       [16:0]   winogradDFT_33_io_output_3;
  wire       [16:0]   winogradDFT_34_io_output_0;
  wire       [16:0]   winogradDFT_34_io_output_1;
  wire       [16:0]   winogradDFT_34_io_output_2;
  wire       [16:0]   winogradDFT_34_io_output_3;
  wire       [16:0]   winogradDFT_35_io_output_0;
  wire       [16:0]   winogradDFT_35_io_output_1;
  wire       [16:0]   winogradDFT_35_io_output_2;
  wire       [16:0]   winogradDFT_35_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_0;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_1;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_2;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_4;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_5;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_6;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_7;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_0;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_1;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_2;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_4;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_5;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_6;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_7;
  wire       [16:0]   _zz_41;
  wire       [25:0]   _zz_42;
  wire       [33:0]   _zz_43;
  wire       [33:0]   _zz_44;
  wire       [16:0]   _zz_45;
  wire       [25:0]   _zz_46;
  wire       [33:0]   _zz_47;
  wire       [33:0]   _zz_48;
  wire       [16:0]   _zz_49;
  wire       [16:0]   _zz_50;
  wire       [25:0]   _zz_51;
  wire       [33:0]   _zz_52;
  wire       [33:0]   _zz_53;
  wire       [16:0]   _zz_54;
  wire       [25:0]   _zz_55;
  wire       [33:0]   _zz_56;
  wire       [33:0]   _zz_57;
  wire       [16:0]   _zz_58;
  wire       [16:0]   _zz_59;
  wire       [25:0]   _zz_60;
  wire       [33:0]   _zz_61;
  wire       [33:0]   _zz_62;
  wire       [16:0]   _zz_63;
  wire       [25:0]   _zz_64;
  wire       [33:0]   _zz_65;
  wire       [33:0]   _zz_66;
  wire       [16:0]   _zz_67;
  wire       [16:0]   _zz_68;
  wire       [25:0]   _zz_69;
  wire       [33:0]   _zz_70;
  wire       [33:0]   _zz_71;
  wire       [16:0]   _zz_72;
  wire       [25:0]   _zz_73;
  wire       [33:0]   _zz_74;
  wire       [33:0]   _zz_75;
  wire       [16:0]   _zz_76;
  wire       [16:0]   _zz_77;
  wire       [25:0]   _zz_78;
  wire       [33:0]   _zz_79;
  wire       [33:0]   _zz_80;
  wire       [16:0]   _zz_81;
  wire       [25:0]   _zz_82;
  wire       [33:0]   _zz_83;
  wire       [33:0]   _zz_84;
  wire       [16:0]   _zz_85;
  wire       [16:0]   _zz_86;
  wire       [25:0]   _zz_87;
  wire       [33:0]   _zz_88;
  wire       [33:0]   _zz_89;
  wire       [16:0]   _zz_90;
  wire       [25:0]   _zz_91;
  wire       [33:0]   _zz_92;
  wire       [33:0]   _zz_93;
  wire       [16:0]   _zz_94;
  wire       [16:0]   _zz_95;
  wire       [25:0]   _zz_96;
  wire       [33:0]   _zz_97;
  wire       [33:0]   _zz_98;
  wire       [16:0]   _zz_99;
  wire       [25:0]   _zz_100;
  wire       [33:0]   _zz_101;
  wire       [33:0]   _zz_102;
  wire       [16:0]   _zz_103;
  wire       [16:0]   _zz_104;
  wire       [25:0]   _zz_105;
  wire       [33:0]   _zz_106;
  wire       [33:0]   _zz_107;
  wire       [16:0]   _zz_108;
  wire       [25:0]   _zz_109;
  wire       [33:0]   _zz_110;
  wire       [33:0]   _zz_111;
  wire       [16:0]   _zz_112;
  wire       [16:0]   _zz_1;
  wire       [16:0]   _zz_2;
  wire       [33:0]   _zz_3;
  wire       [16:0]   _zz_4;
  wire       [16:0]   _zz_5;
  wire       [33:0]   _zz_6;
  wire       [16:0]   _zz_7;
  wire       [16:0]   _zz_8;
  wire       [33:0]   _zz_9;
  wire       [16:0]   _zz_10;
  wire       [16:0]   _zz_11;
  wire       [33:0]   _zz_12;
  wire       [16:0]   _zz_13;
  wire       [16:0]   _zz_14;
  wire       [33:0]   _zz_15;
  wire       [16:0]   _zz_16;
  wire       [16:0]   _zz_17;
  wire       [33:0]   _zz_18;
  wire       [16:0]   _zz_19;
  wire       [16:0]   _zz_20;
  wire       [33:0]   _zz_21;
  wire       [16:0]   _zz_22;
  wire       [16:0]   _zz_23;
  wire       [33:0]   _zz_24;

  assign _zz_41 = ($signed(cooleyTukeyFFT_15_io_output_0) - $signed(cooleyTukeyFFT_15_io_output_1));
  assign _zz_42 = (_zz_43 >>> 8);
  assign _zz_43 = ($signed(_zz_44) + $signed(_zz_3));
  assign _zz_44 = ($signed(_zz_45) * $signed(cooleyTukeyFFT_15_io_output_1));
  assign _zz_45 = ($signed(_zz_1) - $signed(_zz_2));
  assign _zz_46 = (_zz_47 >>> 8);
  assign _zz_47 = ($signed(_zz_48) - $signed(_zz_3));
  assign _zz_48 = ($signed(_zz_49) * $signed(cooleyTukeyFFT_15_io_output_0));
  assign _zz_49 = ($signed(_zz_1) + $signed(_zz_2));
  assign _zz_50 = ($signed(cooleyTukeyFFT_16_io_output_0) - $signed(cooleyTukeyFFT_16_io_output_1));
  assign _zz_51 = (_zz_52 >>> 8);
  assign _zz_52 = ($signed(_zz_53) + $signed(_zz_6));
  assign _zz_53 = ($signed(_zz_54) * $signed(cooleyTukeyFFT_16_io_output_1));
  assign _zz_54 = ($signed(_zz_4) - $signed(_zz_5));
  assign _zz_55 = (_zz_56 >>> 8);
  assign _zz_56 = ($signed(_zz_57) - $signed(_zz_6));
  assign _zz_57 = ($signed(_zz_58) * $signed(cooleyTukeyFFT_16_io_output_0));
  assign _zz_58 = ($signed(_zz_4) + $signed(_zz_5));
  assign _zz_59 = ($signed(cooleyTukeyFFT_15_io_output_2) - $signed(cooleyTukeyFFT_15_io_output_3));
  assign _zz_60 = (_zz_61 >>> 8);
  assign _zz_61 = ($signed(_zz_62) + $signed(_zz_9));
  assign _zz_62 = ($signed(_zz_63) * $signed(cooleyTukeyFFT_15_io_output_3));
  assign _zz_63 = ($signed(_zz_7) - $signed(_zz_8));
  assign _zz_64 = (_zz_65 >>> 8);
  assign _zz_65 = ($signed(_zz_66) - $signed(_zz_9));
  assign _zz_66 = ($signed(_zz_67) * $signed(cooleyTukeyFFT_15_io_output_2));
  assign _zz_67 = ($signed(_zz_7) + $signed(_zz_8));
  assign _zz_68 = ($signed(cooleyTukeyFFT_16_io_output_2) - $signed(cooleyTukeyFFT_16_io_output_3));
  assign _zz_69 = (_zz_70 >>> 8);
  assign _zz_70 = ($signed(_zz_71) + $signed(_zz_12));
  assign _zz_71 = ($signed(_zz_72) * $signed(cooleyTukeyFFT_16_io_output_3));
  assign _zz_72 = ($signed(_zz_10) - $signed(_zz_11));
  assign _zz_73 = (_zz_74 >>> 8);
  assign _zz_74 = ($signed(_zz_75) - $signed(_zz_12));
  assign _zz_75 = ($signed(_zz_76) * $signed(cooleyTukeyFFT_16_io_output_2));
  assign _zz_76 = ($signed(_zz_10) + $signed(_zz_11));
  assign _zz_77 = ($signed(cooleyTukeyFFT_15_io_output_4) - $signed(cooleyTukeyFFT_15_io_output_5));
  assign _zz_78 = (_zz_79 >>> 8);
  assign _zz_79 = ($signed(_zz_80) + $signed(_zz_15));
  assign _zz_80 = ($signed(_zz_81) * $signed(cooleyTukeyFFT_15_io_output_5));
  assign _zz_81 = ($signed(_zz_13) - $signed(_zz_14));
  assign _zz_82 = (_zz_83 >>> 8);
  assign _zz_83 = ($signed(_zz_84) - $signed(_zz_15));
  assign _zz_84 = ($signed(_zz_85) * $signed(cooleyTukeyFFT_15_io_output_4));
  assign _zz_85 = ($signed(_zz_13) + $signed(_zz_14));
  assign _zz_86 = ($signed(cooleyTukeyFFT_16_io_output_4) - $signed(cooleyTukeyFFT_16_io_output_5));
  assign _zz_87 = (_zz_88 >>> 8);
  assign _zz_88 = ($signed(_zz_89) + $signed(_zz_18));
  assign _zz_89 = ($signed(_zz_90) * $signed(cooleyTukeyFFT_16_io_output_5));
  assign _zz_90 = ($signed(_zz_16) - $signed(_zz_17));
  assign _zz_91 = (_zz_92 >>> 8);
  assign _zz_92 = ($signed(_zz_93) - $signed(_zz_18));
  assign _zz_93 = ($signed(_zz_94) * $signed(cooleyTukeyFFT_16_io_output_4));
  assign _zz_94 = ($signed(_zz_16) + $signed(_zz_17));
  assign _zz_95 = ($signed(cooleyTukeyFFT_15_io_output_6) - $signed(cooleyTukeyFFT_15_io_output_7));
  assign _zz_96 = (_zz_97 >>> 8);
  assign _zz_97 = ($signed(_zz_98) + $signed(_zz_21));
  assign _zz_98 = ($signed(_zz_99) * $signed(cooleyTukeyFFT_15_io_output_7));
  assign _zz_99 = ($signed(_zz_19) - $signed(_zz_20));
  assign _zz_100 = (_zz_101 >>> 8);
  assign _zz_101 = ($signed(_zz_102) - $signed(_zz_21));
  assign _zz_102 = ($signed(_zz_103) * $signed(cooleyTukeyFFT_15_io_output_6));
  assign _zz_103 = ($signed(_zz_19) + $signed(_zz_20));
  assign _zz_104 = ($signed(cooleyTukeyFFT_16_io_output_6) - $signed(cooleyTukeyFFT_16_io_output_7));
  assign _zz_105 = (_zz_106 >>> 8);
  assign _zz_106 = ($signed(_zz_107) + $signed(_zz_24));
  assign _zz_107 = ($signed(_zz_108) * $signed(cooleyTukeyFFT_16_io_output_7));
  assign _zz_108 = ($signed(_zz_22) - $signed(_zz_23));
  assign _zz_109 = (_zz_110 >>> 8);
  assign _zz_110 = ($signed(_zz_111) - $signed(_zz_24));
  assign _zz_111 = ($signed(_zz_112) * $signed(cooleyTukeyFFT_16_io_output_6));
  assign _zz_112 = ($signed(_zz_22) + $signed(_zz_23));
  WinogradDFT winogradDFT_32 (
    .io_input_0     (_zz_25[16:0]                      ), //i
    .io_input_1     (_zz_26[16:0]                      ), //i
    .io_input_2     (_zz_27[16:0]                      ), //i
    .io_input_3     (_zz_28[16:0]                      ), //i
    .io_output_0    (winogradDFT_32_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_32_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_32_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_32_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_33 (
    .io_input_0     (_zz_29[16:0]                      ), //i
    .io_input_1     (_zz_30[16:0]                      ), //i
    .io_input_2     (_zz_31[16:0]                      ), //i
    .io_input_3     (_zz_32[16:0]                      ), //i
    .io_output_0    (winogradDFT_33_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_33_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_33_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_33_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_34 (
    .io_input_0     (_zz_33[16:0]                      ), //i
    .io_input_1     (_zz_34[16:0]                      ), //i
    .io_input_2     (_zz_35[16:0]                      ), //i
    .io_input_3     (_zz_36[16:0]                      ), //i
    .io_output_0    (winogradDFT_34_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_34_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_34_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_34_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_35 (
    .io_input_0     (_zz_37[16:0]                      ), //i
    .io_input_1     (_zz_38[16:0]                      ), //i
    .io_input_2     (_zz_39[16:0]                      ), //i
    .io_input_3     (_zz_40[16:0]                      ), //i
    .io_output_0    (winogradDFT_35_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_35_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_35_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_35_io_output_3[16:0]  )  //o
  );
  CooleyTukeyFFT_8 cooleyTukeyFFT_15 (
    .io_input_0     (io_input_0[16:0]                     ), //i
    .io_input_1     (io_input_1[16:0]                     ), //i
    .io_input_2     (io_input_2[16:0]                     ), //i
    .io_input_3     (io_input_3[16:0]                     ), //i
    .io_input_4     (io_input_4[16:0]                     ), //i
    .io_input_5     (io_input_5[16:0]                     ), //i
    .io_input_6     (io_input_6[16:0]                     ), //i
    .io_input_7     (io_input_7[16:0]                     ), //i
    .io_output_0    (cooleyTukeyFFT_15_io_output_0[16:0]  ), //o
    .io_output_1    (cooleyTukeyFFT_15_io_output_1[16:0]  ), //o
    .io_output_2    (cooleyTukeyFFT_15_io_output_2[16:0]  ), //o
    .io_output_3    (cooleyTukeyFFT_15_io_output_3[16:0]  ), //o
    .io_output_4    (cooleyTukeyFFT_15_io_output_4[16:0]  ), //o
    .io_output_5    (cooleyTukeyFFT_15_io_output_5[16:0]  ), //o
    .io_output_6    (cooleyTukeyFFT_15_io_output_6[16:0]  ), //o
    .io_output_7    (cooleyTukeyFFT_15_io_output_7[16:0]  )  //o
  );
  CooleyTukeyFFT_8 cooleyTukeyFFT_16 (
    .io_input_0     (io_input_8[16:0]                     ), //i
    .io_input_1     (io_input_9[16:0]                     ), //i
    .io_input_2     (io_input_10[16:0]                    ), //i
    .io_input_3     (io_input_11[16:0]                    ), //i
    .io_input_4     (io_input_12[16:0]                    ), //i
    .io_input_5     (io_input_13[16:0]                    ), //i
    .io_input_6     (io_input_14[16:0]                    ), //i
    .io_input_7     (io_input_15[16:0]                    ), //i
    .io_output_0    (cooleyTukeyFFT_16_io_output_0[16:0]  ), //o
    .io_output_1    (cooleyTukeyFFT_16_io_output_1[16:0]  ), //o
    .io_output_2    (cooleyTukeyFFT_16_io_output_2[16:0]  ), //o
    .io_output_3    (cooleyTukeyFFT_16_io_output_3[16:0]  ), //o
    .io_output_4    (cooleyTukeyFFT_16_io_output_4[16:0]  ), //o
    .io_output_5    (cooleyTukeyFFT_16_io_output_5[16:0]  ), //o
    .io_output_6    (cooleyTukeyFFT_16_io_output_6[16:0]  ), //o
    .io_output_7    (cooleyTukeyFFT_16_io_output_7[16:0]  )  //o
  );
  assign _zz_1 = 17'h00100;
  assign _zz_2 = 17'h0;
  assign _zz_3 = ($signed(_zz_1) * $signed(_zz_41));
  assign _zz_25 = _zz_42[16:0];
  assign _zz_26 = _zz_46[16:0];
  assign _zz_4 = 17'h00100;
  assign _zz_5 = 17'h0;
  assign _zz_6 = ($signed(_zz_4) * $signed(_zz_50));
  assign _zz_27 = _zz_51[16:0];
  assign _zz_28 = _zz_55[16:0];
  assign _zz_7 = 17'h00100;
  assign _zz_8 = 17'h0;
  assign _zz_9 = ($signed(_zz_7) * $signed(_zz_59));
  assign _zz_29 = _zz_60[16:0];
  assign _zz_30 = _zz_64[16:0];
  assign _zz_10 = 17'h000b5;
  assign _zz_11 = 17'h1ff4b;
  assign _zz_12 = ($signed(_zz_10) * $signed(_zz_68));
  assign _zz_31 = _zz_69[16:0];
  assign _zz_32 = _zz_73[16:0];
  assign _zz_13 = 17'h00100;
  assign _zz_14 = 17'h0;
  assign _zz_15 = ($signed(_zz_13) * $signed(_zz_77));
  assign _zz_33 = _zz_78[16:0];
  assign _zz_34 = _zz_82[16:0];
  assign _zz_16 = 17'h0;
  assign _zz_17 = 17'h1ff00;
  assign _zz_18 = ($signed(_zz_16) * $signed(_zz_86));
  assign _zz_35 = _zz_87[16:0];
  assign _zz_36 = _zz_91[16:0];
  assign _zz_19 = 17'h00100;
  assign _zz_20 = 17'h0;
  assign _zz_21 = ($signed(_zz_19) * $signed(_zz_95));
  assign _zz_37 = _zz_96[16:0];
  assign _zz_38 = _zz_100[16:0];
  assign _zz_22 = 17'h1ff4b;
  assign _zz_23 = 17'h1ff4b;
  assign _zz_24 = ($signed(_zz_22) * $signed(_zz_104));
  assign _zz_39 = _zz_105[16:0];
  assign _zz_40 = _zz_109[16:0];
  assign io_output_0 = winogradDFT_32_io_output_0;
  assign io_output_1 = winogradDFT_32_io_output_1;
  assign io_output_2 = winogradDFT_32_io_output_2;
  assign io_output_3 = winogradDFT_32_io_output_3;
  assign io_output_4 = winogradDFT_33_io_output_0;
  assign io_output_5 = winogradDFT_33_io_output_1;
  assign io_output_6 = winogradDFT_33_io_output_2;
  assign io_output_7 = winogradDFT_33_io_output_3;
  assign io_output_8 = winogradDFT_34_io_output_0;
  assign io_output_9 = winogradDFT_34_io_output_1;
  assign io_output_10 = winogradDFT_34_io_output_2;
  assign io_output_11 = winogradDFT_34_io_output_3;
  assign io_output_12 = winogradDFT_35_io_output_0;
  assign io_output_13 = winogradDFT_35_io_output_1;
  assign io_output_14 = winogradDFT_35_io_output_2;
  assign io_output_15 = winogradDFT_35_io_output_3;

endmodule

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//CooleyTukeyFFT_8 replaced by CooleyTukeyFFT_8

//CooleyTukeyFFT_8 replaced by CooleyTukeyFFT_8

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//CooleyTukeyFFT_8 replaced by CooleyTukeyFFT_8

module CooleyTukeyFFT_8 (
  input      [16:0]   io_input_0,
  input      [16:0]   io_input_1,
  input      [16:0]   io_input_2,
  input      [16:0]   io_input_3,
  input      [16:0]   io_input_4,
  input      [16:0]   io_input_5,
  input      [16:0]   io_input_6,
  input      [16:0]   io_input_7,
  output     [16:0]   io_output_0,
  output     [16:0]   io_output_1,
  output     [16:0]   io_output_2,
  output     [16:0]   io_output_3,
  output     [16:0]   io_output_4,
  output     [16:0]   io_output_5,
  output     [16:0]   io_output_6,
  output     [16:0]   io_output_7
);
  wire       [16:0]   _zz_13;
  wire       [16:0]   _zz_14;
  wire       [16:0]   _zz_15;
  wire       [16:0]   _zz_16;
  wire       [16:0]   _zz_17;
  wire       [16:0]   _zz_18;
  wire       [16:0]   _zz_19;
  wire       [16:0]   _zz_20;
  wire       [16:0]   winogradDFT_32_io_output_0;
  wire       [16:0]   winogradDFT_32_io_output_1;
  wire       [16:0]   winogradDFT_32_io_output_2;
  wire       [16:0]   winogradDFT_32_io_output_3;
  wire       [16:0]   winogradDFT_33_io_output_0;
  wire       [16:0]   winogradDFT_33_io_output_1;
  wire       [16:0]   winogradDFT_33_io_output_2;
  wire       [16:0]   winogradDFT_33_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_0;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_1;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_2;
  wire       [16:0]   cooleyTukeyFFT_15_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_0;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_1;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_2;
  wire       [16:0]   cooleyTukeyFFT_16_io_output_3;
  wire       [16:0]   _zz_21;
  wire       [25:0]   _zz_22;
  wire       [33:0]   _zz_23;
  wire       [33:0]   _zz_24;
  wire       [16:0]   _zz_25;
  wire       [25:0]   _zz_26;
  wire       [33:0]   _zz_27;
  wire       [33:0]   _zz_28;
  wire       [16:0]   _zz_29;
  wire       [16:0]   _zz_30;
  wire       [25:0]   _zz_31;
  wire       [33:0]   _zz_32;
  wire       [33:0]   _zz_33;
  wire       [16:0]   _zz_34;
  wire       [25:0]   _zz_35;
  wire       [33:0]   _zz_36;
  wire       [33:0]   _zz_37;
  wire       [16:0]   _zz_38;
  wire       [16:0]   _zz_39;
  wire       [25:0]   _zz_40;
  wire       [33:0]   _zz_41;
  wire       [33:0]   _zz_42;
  wire       [16:0]   _zz_43;
  wire       [25:0]   _zz_44;
  wire       [33:0]   _zz_45;
  wire       [33:0]   _zz_46;
  wire       [16:0]   _zz_47;
  wire       [16:0]   _zz_48;
  wire       [25:0]   _zz_49;
  wire       [33:0]   _zz_50;
  wire       [33:0]   _zz_51;
  wire       [16:0]   _zz_52;
  wire       [25:0]   _zz_53;
  wire       [33:0]   _zz_54;
  wire       [33:0]   _zz_55;
  wire       [16:0]   _zz_56;
  wire       [16:0]   _zz_1;
  wire       [16:0]   _zz_2;
  wire       [33:0]   _zz_3;
  wire       [16:0]   _zz_4;
  wire       [16:0]   _zz_5;
  wire       [33:0]   _zz_6;
  wire       [16:0]   _zz_7;
  wire       [16:0]   _zz_8;
  wire       [33:0]   _zz_9;
  wire       [16:0]   _zz_10;
  wire       [16:0]   _zz_11;
  wire       [33:0]   _zz_12;

  assign _zz_21 = ($signed(cooleyTukeyFFT_15_io_output_0) - $signed(cooleyTukeyFFT_15_io_output_1));
  assign _zz_22 = (_zz_23 >>> 8);
  assign _zz_23 = ($signed(_zz_24) + $signed(_zz_3));
  assign _zz_24 = ($signed(_zz_25) * $signed(cooleyTukeyFFT_15_io_output_1));
  assign _zz_25 = ($signed(_zz_1) - $signed(_zz_2));
  assign _zz_26 = (_zz_27 >>> 8);
  assign _zz_27 = ($signed(_zz_28) - $signed(_zz_3));
  assign _zz_28 = ($signed(_zz_29) * $signed(cooleyTukeyFFT_15_io_output_0));
  assign _zz_29 = ($signed(_zz_1) + $signed(_zz_2));
  assign _zz_30 = ($signed(cooleyTukeyFFT_16_io_output_0) - $signed(cooleyTukeyFFT_16_io_output_1));
  assign _zz_31 = (_zz_32 >>> 8);
  assign _zz_32 = ($signed(_zz_33) + $signed(_zz_6));
  assign _zz_33 = ($signed(_zz_34) * $signed(cooleyTukeyFFT_16_io_output_1));
  assign _zz_34 = ($signed(_zz_4) - $signed(_zz_5));
  assign _zz_35 = (_zz_36 >>> 8);
  assign _zz_36 = ($signed(_zz_37) - $signed(_zz_6));
  assign _zz_37 = ($signed(_zz_38) * $signed(cooleyTukeyFFT_16_io_output_0));
  assign _zz_38 = ($signed(_zz_4) + $signed(_zz_5));
  assign _zz_39 = ($signed(cooleyTukeyFFT_15_io_output_2) - $signed(cooleyTukeyFFT_15_io_output_3));
  assign _zz_40 = (_zz_41 >>> 8);
  assign _zz_41 = ($signed(_zz_42) + $signed(_zz_9));
  assign _zz_42 = ($signed(_zz_43) * $signed(cooleyTukeyFFT_15_io_output_3));
  assign _zz_43 = ($signed(_zz_7) - $signed(_zz_8));
  assign _zz_44 = (_zz_45 >>> 8);
  assign _zz_45 = ($signed(_zz_46) - $signed(_zz_9));
  assign _zz_46 = ($signed(_zz_47) * $signed(cooleyTukeyFFT_15_io_output_2));
  assign _zz_47 = ($signed(_zz_7) + $signed(_zz_8));
  assign _zz_48 = ($signed(cooleyTukeyFFT_16_io_output_2) - $signed(cooleyTukeyFFT_16_io_output_3));
  assign _zz_49 = (_zz_50 >>> 8);
  assign _zz_50 = ($signed(_zz_51) + $signed(_zz_12));
  assign _zz_51 = ($signed(_zz_52) * $signed(cooleyTukeyFFT_16_io_output_3));
  assign _zz_52 = ($signed(_zz_10) - $signed(_zz_11));
  assign _zz_53 = (_zz_54 >>> 8);
  assign _zz_54 = ($signed(_zz_55) - $signed(_zz_12));
  assign _zz_55 = ($signed(_zz_56) * $signed(cooleyTukeyFFT_16_io_output_2));
  assign _zz_56 = ($signed(_zz_10) + $signed(_zz_11));
  WinogradDFT winogradDFT_32 (
    .io_input_0     (_zz_13[16:0]                      ), //i
    .io_input_1     (_zz_14[16:0]                      ), //i
    .io_input_2     (_zz_15[16:0]                      ), //i
    .io_input_3     (_zz_16[16:0]                      ), //i
    .io_output_0    (winogradDFT_32_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_32_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_32_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_32_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_33 (
    .io_input_0     (_zz_17[16:0]                      ), //i
    .io_input_1     (_zz_18[16:0]                      ), //i
    .io_input_2     (_zz_19[16:0]                      ), //i
    .io_input_3     (_zz_20[16:0]                      ), //i
    .io_output_0    (winogradDFT_33_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_33_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_33_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_33_io_output_3[16:0]  )  //o
  );
  CooleyTukeyFFT cooleyTukeyFFT_15 (
    .io_input_0     (io_input_0[16:0]                     ), //i
    .io_input_1     (io_input_1[16:0]                     ), //i
    .io_input_2     (io_input_2[16:0]                     ), //i
    .io_input_3     (io_input_3[16:0]                     ), //i
    .io_output_0    (cooleyTukeyFFT_15_io_output_0[16:0]  ), //o
    .io_output_1    (cooleyTukeyFFT_15_io_output_1[16:0]  ), //o
    .io_output_2    (cooleyTukeyFFT_15_io_output_2[16:0]  ), //o
    .io_output_3    (cooleyTukeyFFT_15_io_output_3[16:0]  )  //o
  );
  CooleyTukeyFFT cooleyTukeyFFT_16 (
    .io_input_0     (io_input_4[16:0]                     ), //i
    .io_input_1     (io_input_5[16:0]                     ), //i
    .io_input_2     (io_input_6[16:0]                     ), //i
    .io_input_3     (io_input_7[16:0]                     ), //i
    .io_output_0    (cooleyTukeyFFT_16_io_output_0[16:0]  ), //o
    .io_output_1    (cooleyTukeyFFT_16_io_output_1[16:0]  ), //o
    .io_output_2    (cooleyTukeyFFT_16_io_output_2[16:0]  ), //o
    .io_output_3    (cooleyTukeyFFT_16_io_output_3[16:0]  )  //o
  );
  assign _zz_1 = 17'h00100;
  assign _zz_2 = 17'h0;
  assign _zz_3 = ($signed(_zz_1) * $signed(_zz_21));
  assign _zz_13 = _zz_22[16:0];
  assign _zz_14 = _zz_26[16:0];
  assign _zz_4 = 17'h00100;
  assign _zz_5 = 17'h0;
  assign _zz_6 = ($signed(_zz_4) * $signed(_zz_30));
  assign _zz_15 = _zz_31[16:0];
  assign _zz_16 = _zz_35[16:0];
  assign _zz_7 = 17'h00100;
  assign _zz_8 = 17'h0;
  assign _zz_9 = ($signed(_zz_7) * $signed(_zz_39));
  assign _zz_17 = _zz_40[16:0];
  assign _zz_18 = _zz_44[16:0];
  assign _zz_10 = 17'h0;
  assign _zz_11 = 17'h1ff00;
  assign _zz_12 = ($signed(_zz_10) * $signed(_zz_48));
  assign _zz_19 = _zz_49[16:0];
  assign _zz_20 = _zz_53[16:0];
  assign io_output_0 = winogradDFT_32_io_output_0;
  assign io_output_1 = winogradDFT_32_io_output_1;
  assign io_output_2 = winogradDFT_32_io_output_2;
  assign io_output_3 = winogradDFT_32_io_output_3;
  assign io_output_4 = winogradDFT_33_io_output_0;
  assign io_output_5 = winogradDFT_33_io_output_1;
  assign io_output_6 = winogradDFT_33_io_output_2;
  assign io_output_7 = winogradDFT_33_io_output_3;

endmodule

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//CooleyTukeyFFT replaced by CooleyTukeyFFT

//CooleyTukeyFFT replaced by CooleyTukeyFFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//CooleyTukeyFFT replaced by CooleyTukeyFFT

//CooleyTukeyFFT replaced by CooleyTukeyFFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//CooleyTukeyFFT replaced by CooleyTukeyFFT

//CooleyTukeyFFT replaced by CooleyTukeyFFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//CooleyTukeyFFT replaced by CooleyTukeyFFT

module CooleyTukeyFFT (
  input      [16:0]   io_input_0,
  input      [16:0]   io_input_1,
  input      [16:0]   io_input_2,
  input      [16:0]   io_input_3,
  output     [16:0]   io_output_0,
  output     [16:0]   io_output_1,
  output     [16:0]   io_output_2,
  output     [16:0]   io_output_3
);
  wire       [16:0]   winogradDFT_32_io_output_0;
  wire       [16:0]   winogradDFT_32_io_output_1;
  wire       [16:0]   winogradDFT_32_io_output_2;
  wire       [16:0]   winogradDFT_32_io_output_3;

  WinogradDFT winogradDFT_32 (
    .io_input_0     (io_input_0[16:0]                  ), //i
    .io_input_1     (io_input_1[16:0]                  ), //i
    .io_input_2     (io_input_2[16:0]                  ), //i
    .io_input_3     (io_input_3[16:0]                  ), //i
    .io_output_0    (winogradDFT_32_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_32_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_32_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_32_io_output_3[16:0]  )  //o
  );
  assign io_output_0 = winogradDFT_32_io_output_0;
  assign io_output_1 = winogradDFT_32_io_output_1;
  assign io_output_2 = winogradDFT_32_io_output_2;
  assign io_output_3 = winogradDFT_32_io_output_3;

endmodule

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

module WinogradDFT (
  input      [16:0]   io_input_0,
  input      [16:0]   io_input_1,
  input      [16:0]   io_input_2,
  input      [16:0]   io_input_3,
  output     [16:0]   io_output_0,
  output     [16:0]   io_output_1,
  output     [16:0]   io_output_2,
  output     [16:0]   io_output_3
);

  assign io_output_0 = ($signed(io_input_0) + $signed(io_input_2));
  assign io_output_1 = ($signed(io_input_1) + $signed(io_input_3));
  assign io_output_2 = ($signed(io_input_0) - $signed(io_input_2));
  assign io_output_3 = ($signed(io_input_1) - $signed(io_input_3));

endmodule
