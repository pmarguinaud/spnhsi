
ifeq ($(SRC), )
SRC=.
endif

ifeq ($(BUILD), )
BUILD=.
endif

all: exe

include Makefile.inc

$(BUILD)/abor1.intfb.ok: $(BUILD)/abor1.intfb.h 
	touch $(BUILD)/abor1.intfb.ok

$(BUILD)/mxmaop.intfb.ok: $(BUILD)/mxmaop.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/mxmaop.intfb.ok

$(BUILD)/mxmaoptr.intfb.ok: $(BUILD)/mxmaoptr.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/mxmaoptr.intfb.ok

$(BUILD)/mxptma.intfb.ok: $(BUILD)/mxptma.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/mxptma.intfb.ok

$(BUILD)/mxture.intfb.ok: $(BUILD)/mxture.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/mxture.intfb.ok

$(BUILD)/sgemmx.intfb.ok: $(BUILD)/sgemmx.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/sgemmx.intfb.ok

$(BUILD)/si_cccor.intfb.ok: $(BUILD)/si_cccor.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o
	touch $(BUILD)/si_cccor.intfb.ok

$(BUILD)/si_mxptco.intfb.ok: $(BUILD)/si_mxptco.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/si_mxptco.intfb.ok

$(BUILD)/sidd.intfb.ok: $(BUILD)/sidd.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o
	touch $(BUILD)/sidd.intfb.ok

$(BUILD)/sigam.intfb.ok: $(BUILD)/sigam.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o
	touch $(BUILD)/sigam.intfb.ok

$(BUILD)/sigam_nh.intfb.ok: $(BUILD)/sigam_nh.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o
	touch $(BUILD)/sigam_nh.intfb.ok

$(BUILD)/simplico.intfb.ok: $(BUILD)/simplico.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/simplico.intfb.ok

$(BUILD)/siptp.intfb.ok: $(BUILD)/siptp.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o
	touch $(BUILD)/siptp.intfb.ok

$(BUILD)/siseve.intfb.ok: $(BUILD)/siseve.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o
	touch $(BUILD)/siseve.intfb.ok

$(BUILD)/siskapi.intfb.ok: $(BUILD)/siskapi.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o $(BUILD)/parkind1.o
	touch $(BUILD)/siskapi.intfb.ok

$(BUILD)/sitnu.intfb.ok: $(BUILD)/sitnu.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o
	touch $(BUILD)/sitnu.intfb.ok

$(BUILD)/spcsidg_part0nh.intfb.ok: $(BUILD)/spcsidg_part0nh.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/yomdyn.o $(BUILD)/parkind1.o
	touch $(BUILD)/spcsidg_part0nh.intfb.ok

$(BUILD)/spcsidg_part1.intfb.ok: $(BUILD)/spcsidg_part1.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomdyn.o
	touch $(BUILD)/spcsidg_part1.intfb.ok

$(BUILD)/spcsidg_part2.intfb.ok: $(BUILD)/spcsidg_part2.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o
	touch $(BUILD)/spcsidg_part2.intfb.ok

$(BUILD)/spnhsi.intfb.ok: $(BUILD)/spnhsi.intfb.h $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o $(BUILD)/yomlddh.o $(BUILD)/yomrip.o
	touch $(BUILD)/spnhsi.intfb.ok

$(BUILD)/tridia.intfb.ok: $(BUILD)/tridia.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/tridia.intfb.ok

$(BUILD)/verder.intfb.ok: $(BUILD)/verder.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/verder.intfb.ok

$(BUILD)/verderfe.intfb.ok: $(BUILD)/verderfe.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/verderfe.intfb.ok

$(BUILD)/verdisint.intfb.ok: $(BUILD)/verdisint.intfb.h $(BUILD)/parkind1.o $(BUILD)/yomcver.o $(BUILD)/yomvert.o
	touch $(BUILD)/verdisint.intfb.ok

$(BUILD)/verint.intfb.ok: $(BUILD)/verint.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/verint.intfb.ok

$(BUILD)/verints.intfb.ok: $(BUILD)/verints.intfb.h $(BUILD)/parkind1.o
	touch $(BUILD)/verints.intfb.ok

$(BUILD)/mxmaop.ok: $(SRC)/mxmaop.h $(BUILD)/parkind1.o
	touch $(BUILD)/mxmaop.ok

$(BUILD)/mxmaoptr.ok: $(SRC)/mxmaoptr.h $(BUILD)/parkind1.o
	touch $(BUILD)/mxmaoptr.ok

$(BUILD)/mxptma.ok: $(SRC)/mxptma.h $(BUILD)/parkind1.o
	touch $(BUILD)/mxptma.ok

$(BUILD)/mxture.ok: $(SRC)/mxture.h $(BUILD)/parkind1.o
	touch $(BUILD)/mxture.ok

$(BUILD)/si_mxptco.ok: $(SRC)/si_mxptco.h $(BUILD)/parkind1.o
	touch $(BUILD)/si_mxptco.ok

$(BUILD)/simplico.ok: $(SRC)/simplico.h $(BUILD)/parkind1.o
	touch $(BUILD)/simplico.ok

$(BUILD)/tridia.ok: $(SRC)/tridia.h $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	touch $(BUILD)/tridia.ok

$(BUILD)/abor1.o: $(SRC)/abor1.F90 
	$(FC) -o $(BUILD)/abor1.o -c $(SRC)/abor1.F90

$(BUILD)/geometry_mod.o: $(SRC)/geometry_mod.F90 $(BUILD)/parkind1.o $(BUILD)/type_geometry.o
	$(FC) -o $(BUILD)/geometry_mod.o -c $(SRC)/geometry_mod.F90

$(BUILD)/intdyn_mod.o: $(SRC)/intdyn_mod.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/geometry_mod.o
	$(FC) -o $(BUILD)/intdyn_mod.o -c $(SRC)/intdyn_mod.F90

$(BUILD)/main_spnhsi.o: $(SRC)/main_spnhsi.F90 $(BUILD)/xrd_getoptions.o $(BUILD)/xrd_unix_env.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yommp0.o $(BUILD)/yomct0.o $(BUILD)/geometry_mod.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o $(BUILD)/yomlddh.o $(BUILD)/yomrip.o $(BUILD)/util_geometry_mod.o $(BUILD)/util_tcst_mod.o $(BUILD)/util_tdyn_mod.o $(BUILD)/util_tdyna_mod.o $(BUILD)/util_tlddh_mod.o $(BUILD)/util_trip_mod.o $(BUILD)/yomdata.o spnhsi.intfb.ok
	$(FC) -o $(BUILD)/main_spnhsi.o -c $(SRC)/main_spnhsi.F90

$(BUILD)/mxmaop.o: $(SRC)/mxmaop.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/oml_mod.o
	$(FC) -o $(BUILD)/mxmaop.o -c $(SRC)/mxmaop.F90

$(BUILD)/mxmaoptr.o: $(SRC)/mxmaoptr.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/oml_mod.o mxmaop.ok
	$(FC) -o $(BUILD)/mxmaoptr.o -c $(SRC)/mxmaoptr.F90

$(BUILD)/mxptma.o: $(SRC)/mxptma.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/mxptma.o -c $(SRC)/mxptma.F90

$(BUILD)/mxture.o: $(SRC)/mxture.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/mxture.o -c $(SRC)/mxture.F90

$(BUILD)/oml_mod.o: $(SRC)/oml_mod.F90 $(BUILD)/parkind1.o $(BUILD)/yomlun.o
	$(FC) -o $(BUILD)/oml_mod.o -c $(SRC)/oml_mod.F90

$(BUILD)/parkind1.o: $(SRC)/parkind1.F90 
	$(FC) -o $(BUILD)/parkind1.o -c $(SRC)/parkind1.F90

$(BUILD)/reglatlon_field_mix.o: $(SRC)/reglatlon_field_mix.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/reglatlon_field_mix.o -c $(SRC)/reglatlon_field_mix.F90

$(BUILD)/sgemmx.o: $(SRC)/sgemmx.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/sgemmx.o -c $(SRC)/sgemmx.F90

$(BUILD)/si_cccor.o: $(SRC)/si_cccor.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o sigam.intfb.ok sitnu.intfb.ok siseve.intfb.ok
	$(FC) -o $(BUILD)/si_cccor.o -c $(SRC)/si_cccor.F90

$(BUILD)/si_mxptco.o: $(SRC)/si_mxptco.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/si_mxptco.o -c $(SRC)/si_mxptco.F90

$(BUILD)/sidd.o: $(SRC)/sidd.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o siseve.intfb.ok sigam.intfb.ok sigam_nh.intfb.ok
	$(FC) -o $(BUILD)/sidd.o -c $(SRC)/sidd.F90

$(BUILD)/sigam.o: $(SRC)/sigam.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o verdisint.intfb.ok
	$(FC) -o $(BUILD)/sigam.o -c $(SRC)/sigam.F90

$(BUILD)/sigam_nh.o: $(SRC)/sigam_nh.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o verdisint.intfb.ok
	$(FC) -o $(BUILD)/sigam_nh.o -c $(SRC)/sigam_nh.F90

$(BUILD)/simplico.o: $(SRC)/simplico.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/simplico.o -c $(SRC)/simplico.F90

$(BUILD)/siptp.o: $(SRC)/siptp.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o sitnu.intfb.ok
	$(FC) -o $(BUILD)/siptp.o -c $(SRC)/siptp.F90

$(BUILD)/siseve.o: $(SRC)/siseve.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o verdisint.intfb.ok verderfe.intfb.ok siskapi.intfb.ok
	$(FC) -o $(BUILD)/siseve.o -c $(SRC)/siseve.F90

$(BUILD)/siskapi.o: $(SRC)/siskapi.F90 $(BUILD)/geometry_mod.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/siskapi.o -c $(SRC)/siskapi.F90

$(BUILD)/sitnu.o: $(SRC)/sitnu.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o $(BUILD)/yomdyn.o verdisint.intfb.ok
	$(FC) -o $(BUILD)/sitnu.o -c $(SRC)/sitnu.F90

$(BUILD)/spcsidg_part0nh.o: $(SRC)/spcsidg_part0nh.F90 $(BUILD)/geometry_mod.o $(BUILD)/yomdyn.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yommp0.o
	$(FC) -o $(BUILD)/spcsidg_part0nh.o -c $(SRC)/spcsidg_part0nh.F90

$(BUILD)/spcsidg_part1.o: $(SRC)/spcsidg_part1.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomdyn.o $(BUILD)/yommp0.o mxture.ok
	$(FC) -o $(BUILD)/spcsidg_part1.o -c $(SRC)/spcsidg_part1.F90

$(BUILD)/spcsidg_part2.o: $(SRC)/spcsidg_part2.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o mxptma.ok
	$(FC) -o $(BUILD)/spcsidg_part2.o -c $(SRC)/spcsidg_part2.F90

$(BUILD)/spnhsi.o: $(SRC)/spnhsi.F90 $(BUILD)/geometry_mod.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o $(BUILD)/yommp0.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o $(BUILD)/yomlddh.o $(BUILD)/yomrip.o $(BUILD)/yomct0.o mxmaoptr.ok siseve.intfb.ok si_cccor.intfb.ok sidd.intfb.ok sigam.intfb.ok simplico.ok si_mxptco.ok siptp.intfb.ok sitnu.intfb.ok spcsidg_part0nh.intfb.ok spcsidg_part1.intfb.ok spcsidg_part2.intfb.ok
	$(FC) -o $(BUILD)/spnhsi.o -c $(SRC)/spnhsi.F90

$(BUILD)/tridia.o: $(SRC)/tridia.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/tridia.o -c $(SRC)/tridia.F90

$(BUILD)/type_geometry.o: $(SRC)/type_geometry.F90 $(BUILD)/yomvert.o $(BUILD)/yomsta.o $(BUILD)/yomlap.o $(BUILD)/yomleg.o $(BUILD)/yomdim.o $(BUILD)/yomdimv.o $(BUILD)/yommp.o $(BUILD)/yomgem.o $(BUILD)/type_spgeom.o $(BUILD)/yemdim.o $(BUILD)/yemgeo.o $(BUILD)/yemmp.o $(BUILD)/yemlap.o $(BUILD)/yemgsl.o $(BUILD)/yomcver.o
	$(FC) -o $(BUILD)/type_geometry.o -c $(SRC)/type_geometry.F90

$(BUILD)/type_spgeom.o: $(SRC)/type_spgeom.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/type_spgeom.o -c $(SRC)/type_spgeom.F90

$(BUILD)/util_geometry_mod.o: $(SRC)/util_geometry_mod.F90 $(BUILD)/type_geometry.o $(BUILD)/util_tcsgleg_mod.o $(BUILD)/util_tdimv_mod.o $(BUILD)/util_tdim_mod.o $(BUILD)/util_tedim_mod.o $(BUILD)/util_tegeo_mod.o $(BUILD)/util_tegsl_mod.o $(BUILD)/util_temmp_mod.o $(BUILD)/util_tgem_mod.o $(BUILD)/util_tlap_mod.o $(BUILD)/util_tlep_mod.o $(BUILD)/util_tmp_mod.o $(BUILD)/util_tspgeom_mod.o $(BUILD)/util_tsta_mod.o $(BUILD)/util_tvab_mod.o $(BUILD)/util_tvertical_geom_mod.o
	$(FC) -o $(BUILD)/util_geometry_mod.o -c $(SRC)/util_geometry_mod.F90

$(BUILD)/util_reglatlon_field_mod.o: $(SRC)/util_reglatlon_field_mod.F90 $(BUILD)/reglatlon_field_mix.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_reglatlon_field_mod.o -c $(SRC)/util_reglatlon_field_mod.F90

$(BUILD)/util_tcsgleg_mod.o: $(SRC)/util_tcsgleg_mod.F90 $(BUILD)/yomleg.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tcsgleg_mod.o -c $(SRC)/util_tcsgleg_mod.F90

$(BUILD)/util_tcst_mod.o: $(SRC)/util_tcst_mod.F90 $(BUILD)/yomcst.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tcst_mod.o -c $(SRC)/util_tcst_mod.F90

$(BUILD)/util_tcty_mod.o: $(SRC)/util_tcty_mod.F90 $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/util_tcty_mod.o -c $(SRC)/util_tcty_mod.F90

$(BUILD)/util_tcver_mod.o: $(SRC)/util_tcver_mod.F90 $(BUILD)/yomcver.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tcver_mod.o -c $(SRC)/util_tcver_mod.F90

$(BUILD)/util_tdim_mod.o: $(SRC)/util_tdim_mod.F90 $(BUILD)/yomdim.o
	$(FC) -o $(BUILD)/util_tdim_mod.o -c $(SRC)/util_tdim_mod.F90

$(BUILD)/util_tdimv_mod.o: $(SRC)/util_tdimv_mod.F90 $(BUILD)/yomdimv.o
	$(FC) -o $(BUILD)/util_tdimv_mod.o -c $(SRC)/util_tdimv_mod.F90

$(BUILD)/util_tdyn_mod.o: $(SRC)/util_tdyn_mod.F90 $(BUILD)/yomdyn.o $(BUILD)/util_tswe_mod.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tdyn_mod.o -c $(SRC)/util_tdyn_mod.F90

$(BUILD)/util_tdyna_mod.o: $(SRC)/util_tdyna_mod.F90 $(BUILD)/yomdyna.o $(BUILD)/util_tgflt_mod.o $(BUILD)/util_tgmvt_mod.o $(BUILD)/util_ttnd_mod.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tdyna_mod.o -c $(SRC)/util_tdyna_mod.F90

$(BUILD)/util_teaerc_mod.o: $(SRC)/util_teaerc_mod.F90 $(BUILD)/yoeaerc.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_teaerc_mod.o -c $(SRC)/util_teaerc_mod.F90

$(BUILD)/util_teaerc_tegen_mod.o: $(SRC)/util_teaerc_tegen_mod.F90 $(BUILD)/yoeaerc_tegen.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_teaerc_tegen_mod.o -c $(SRC)/util_teaerc_tegen_mod.F90

$(BUILD)/util_tecmip_mod.o: $(SRC)/util_tecmip_mod.F90 $(BUILD)/yoecmip.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tecmip_mod.o -c $(SRC)/util_tecmip_mod.F90

$(BUILD)/util_tedim_mod.o: $(SRC)/util_tedim_mod.F90 $(BUILD)/yemdim.o
	$(FC) -o $(BUILD)/util_tedim_mod.o -c $(SRC)/util_tedim_mod.F90

$(BUILD)/util_tegeo_mod.o: $(SRC)/util_tegeo_mod.F90 $(BUILD)/yemgeo.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tegeo_mod.o -c $(SRC)/util_tegeo_mod.F90

$(BUILD)/util_tegsl_mod.o: $(SRC)/util_tegsl_mod.F90 $(BUILD)/yemgsl.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tegsl_mod.o -c $(SRC)/util_tegsl_mod.F90

$(BUILD)/util_temmp_mod.o: $(SRC)/util_temmp_mod.F90 $(BUILD)/yemmp.o
	$(FC) -o $(BUILD)/util_temmp_mod.o -c $(SRC)/util_temmp_mod.F90

$(BUILD)/util_teozoc_mod.o: $(SRC)/util_teozoc_mod.F90 $(BUILD)/yoeozoc.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_teozoc_mod.o -c $(SRC)/util_teozoc_mod.F90

$(BUILD)/util_tgem_mod.o: $(SRC)/util_tgem_mod.F90 $(BUILD)/yomgem.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tgem_mod.o -c $(SRC)/util_tgem_mod.F90

$(BUILD)/util_tgflt_mod.o: $(SRC)/util_tgflt_mod.F90 $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/util_tgflt_mod.o -c $(SRC)/util_tgflt_mod.F90

$(BUILD)/util_tgmvt_mod.o: $(SRC)/util_tgmvt_mod.F90 $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/util_tgmvt_mod.o -c $(SRC)/util_tgmvt_mod.F90

$(BUILD)/util_thwind_mod.o: $(SRC)/util_thwind_mod.F90 $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/util_thwind_mod.o -c $(SRC)/util_thwind_mod.F90

$(BUILD)/util_tlap_mod.o: $(SRC)/util_tlap_mod.F90 $(BUILD)/yomlap.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tlap_mod.o -c $(SRC)/util_tlap_mod.F90

$(BUILD)/util_tlddh_mod.o: $(SRC)/util_tlddh_mod.F90 $(BUILD)/yomlddh.o
	$(FC) -o $(BUILD)/util_tlddh_mod.o -c $(SRC)/util_tlddh_mod.F90

$(BUILD)/util_tlep_mod.o: $(SRC)/util_tlep_mod.F90 $(BUILD)/yemlap.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tlep_mod.o -c $(SRC)/util_tlep_mod.F90

$(BUILD)/util_tmp_mod.o: $(SRC)/util_tmp_mod.F90 $(BUILD)/yommp.o
	$(FC) -o $(BUILD)/util_tmp_mod.o -c $(SRC)/util_tmp_mod.F90

$(BUILD)/util_tradghg_mod.o: $(SRC)/util_tradghg_mod.F90 $(BUILD)/yoeradghg.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tradghg_mod.o -c $(SRC)/util_tradghg_mod.F90

$(BUILD)/util_trcp_mod.o: $(SRC)/util_trcp_mod.F90 $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/util_trcp_mod.o -c $(SRC)/util_trcp_mod.F90

$(BUILD)/util_trip_mod.o: $(SRC)/util_trip_mod.F90 $(BUILD)/yomrip.o $(BUILD)/util_reglatlon_field_mod.o $(BUILD)/util_teaerc_mod.o $(BUILD)/util_teaerc_tegen_mod.o $(BUILD)/util_tecmip_mod.o $(BUILD)/util_teozoc_mod.o $(BUILD)/util_tradghg_mod.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_trip_mod.o -c $(SRC)/util_trip_mod.F90

$(BUILD)/util_tspgeom_mod.o: $(SRC)/util_tspgeom_mod.F90 $(BUILD)/type_spgeom.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tspgeom_mod.o -c $(SRC)/util_tspgeom_mod.F90

$(BUILD)/util_tsta_mod.o: $(SRC)/util_tsta_mod.F90 $(BUILD)/yomsta.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tsta_mod.o -c $(SRC)/util_tsta_mod.F90

$(BUILD)/util_tswe_mod.o: $(SRC)/util_tswe_mod.F90 $(BUILD)/yomswe.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tswe_mod.o -c $(SRC)/util_tswe_mod.F90

$(BUILD)/util_ttnd_mod.o: $(SRC)/util_ttnd_mod.F90 $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/util_ttnd_mod.o -c $(SRC)/util_ttnd_mod.F90

$(BUILD)/util_tvab_mod.o: $(SRC)/util_tvab_mod.F90 $(BUILD)/yomvert.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tvab_mod.o -c $(SRC)/util_tvab_mod.F90

$(BUILD)/util_tvertical_geom_mod.o: $(SRC)/util_tvertical_geom_mod.F90 $(BUILD)/yomvert.o $(BUILD)/util_tcver_mod.o $(BUILD)/util_tvab_mod.o $(BUILD)/util_tveta_mod.o $(BUILD)/util_tvfe_mod.o
	$(FC) -o $(BUILD)/util_tvertical_geom_mod.o -c $(SRC)/util_tvertical_geom_mod.F90

$(BUILD)/util_tveta_mod.o: $(SRC)/util_tveta_mod.F90 $(BUILD)/yomvert.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tveta_mod.o -c $(SRC)/util_tveta_mod.F90

$(BUILD)/util_tvfe_mod.o: $(SRC)/util_tvfe_mod.F90 $(BUILD)/yomvert.o $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/util_tvfe_mod.o -c $(SRC)/util_tvfe_mod.F90

$(BUILD)/util_txyb_mod.o: $(SRC)/util_txyb_mod.F90 $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/util_txyb_mod.o -c $(SRC)/util_txyb_mod.F90

$(BUILD)/util_txybder_mod.o: $(SRC)/util_txybder_mod.F90 $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/util_txybder_mod.o -c $(SRC)/util_txybder_mod.F90

$(BUILD)/verder.o: $(SRC)/verder.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/verder.o -c $(SRC)/verder.F90

$(BUILD)/verderfe.o: $(SRC)/verderfe.F90 $(BUILD)/yomlun.o $(BUILD)/parkind1.o $(BUILD)/yomhook.o tridia.ok
	$(FC) -o $(BUILD)/verderfe.o -c $(SRC)/verderfe.F90

$(BUILD)/verdisint.o: $(SRC)/verdisint.F90 $(BUILD)/parkind1.o $(BUILD)/yomcver.o $(BUILD)/yomhook.o $(BUILD)/yomlun.o $(BUILD)/yomvert.o $(BUILD)/oml_mod.o verder.intfb.ok verint.intfb.ok verints.intfb.ok
	$(FC) -o $(BUILD)/verdisint.o -c $(SRC)/verdisint.F90

$(BUILD)/verint.o: $(SRC)/verint.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomlun.o $(BUILD)/oml_mod.o
	$(FC) -o $(BUILD)/verint.o -c $(SRC)/verint.F90

$(BUILD)/verints.o: $(SRC)/verints.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/verints.o -c $(SRC)/verints.F90

$(BUILD)/xrd_getoptions.o: $(SRC)/xrd_getoptions.F90 $(BUILD)/parkind1.o $(BUILD)/xrd_unix_env.o
	$(FC) -o $(BUILD)/xrd_getoptions.o -c $(SRC)/xrd_getoptions.F90

$(BUILD)/xrd_unix_env.o: $(SRC)/xrd_unix_env.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/xrd_unix_env.o -c $(SRC)/xrd_unix_env.F90

$(BUILD)/yemdim.o: $(SRC)/yemdim.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yemdim.o -c $(SRC)/yemdim.F90

$(BUILD)/yemgeo.o: $(SRC)/yemgeo.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yemgeo.o -c $(SRC)/yemgeo.F90

$(BUILD)/yemgsl.o: $(SRC)/yemgsl.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yemgsl.o -c $(SRC)/yemgsl.F90

$(BUILD)/yemlap.o: $(SRC)/yemlap.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yemlap.o -c $(SRC)/yemlap.F90

$(BUILD)/yemmp.o: $(SRC)/yemmp.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yemmp.o -c $(SRC)/yemmp.F90

$(BUILD)/yoeaerc.o: $(SRC)/yoeaerc.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yoeaerc.o -c $(SRC)/yoeaerc.F90

$(BUILD)/yoeaerc_tegen.o: $(SRC)/yoeaerc_tegen.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yoeaerc_tegen.o -c $(SRC)/yoeaerc_tegen.F90

$(BUILD)/yoecmip.o: $(SRC)/yoecmip.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yoecmip.o -c $(SRC)/yoecmip.F90

$(BUILD)/yoeozoc.o: $(SRC)/yoeozoc.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yoeozoc.o -c $(SRC)/yoeozoc.F90

$(BUILD)/yoeradghg.o: $(SRC)/yoeradghg.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcst.o
	$(FC) -o $(BUILD)/yoeradghg.o -c $(SRC)/yoeradghg.F90

$(BUILD)/yomcst.o: $(SRC)/yomcst.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomcst.o -c $(SRC)/yomcst.F90

$(BUILD)/yomct0.o: $(SRC)/yomct0.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomct0.o -c $(SRC)/yomct0.F90

$(BUILD)/yomcver.o: $(SRC)/yomcver.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/yomcver.o -c $(SRC)/yomcver.F90

$(BUILD)/yomdata.o: $(SRC)/yomdata.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomdata.o -c $(SRC)/yomdata.F90

$(BUILD)/yomdim.o: $(SRC)/yomdim.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomdim.o -c $(SRC)/yomdim.F90

$(BUILD)/yomdimv.o: $(SRC)/yomdimv.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomdimv.o -c $(SRC)/yomdimv.F90

$(BUILD)/yomdyn.o: $(SRC)/yomdyn.F90 $(BUILD)/parkind1.o $(BUILD)/yomswe.o $(BUILD)/yomhook.o
	$(FC) -o $(BUILD)/yomdyn.o -c $(SRC)/yomdyn.F90

$(BUILD)/yomdyna.o: $(SRC)/yomdyna.F90 $(BUILD)/parkind1.o $(BUILD)/intdyn_mod.o
	$(FC) -o $(BUILD)/yomdyna.o -c $(SRC)/yomdyna.F90

$(BUILD)/yomgem.o: $(SRC)/yomgem.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomgem.o -c $(SRC)/yomgem.F90

$(BUILD)/yomhook.o: $(SRC)/yomhook.F90 
	$(FC) -o $(BUILD)/yomhook.o -c $(SRC)/yomhook.F90

$(BUILD)/yomlap.o: $(SRC)/yomlap.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomlap.o -c $(SRC)/yomlap.F90

$(BUILD)/yomlddh.o: $(SRC)/yomlddh.F90 
	$(FC) -o $(BUILD)/yomlddh.o -c $(SRC)/yomlddh.F90

$(BUILD)/yomleg.o: $(SRC)/yomleg.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomleg.o -c $(SRC)/yomleg.F90

$(BUILD)/yomlun.o: $(SRC)/yomlun.F90 
	$(FC) -o $(BUILD)/yomlun.o -c $(SRC)/yomlun.F90

$(BUILD)/yommp.o: $(SRC)/yommp.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yommp.o -c $(SRC)/yommp.F90

$(BUILD)/yommp0.o: $(SRC)/yommp0.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yommp0.o -c $(SRC)/yommp0.F90

$(BUILD)/yomrip.o: $(SRC)/yomrip.F90 $(BUILD)/parkind1.o $(BUILD)/yoeozoc.o $(BUILD)/yoecmip.o $(BUILD)/yoeradghg.o $(BUILD)/yoeaerc_tegen.o $(BUILD)/yoeaerc.o $(BUILD)/reglatlon_field_mix.o
	$(FC) -o $(BUILD)/yomrip.o -c $(SRC)/yomrip.F90

$(BUILD)/yomsta.o: $(SRC)/yomsta.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomsta.o -c $(SRC)/yomsta.F90

$(BUILD)/yomswe.o: $(SRC)/yomswe.F90 $(BUILD)/parkind1.o
	$(FC) -o $(BUILD)/yomswe.o -c $(SRC)/yomswe.F90

$(BUILD)/yomvert.o: $(SRC)/yomvert.F90 $(BUILD)/parkind1.o $(BUILD)/yomhook.o $(BUILD)/yomcver.o
	$(FC) -o $(BUILD)/yomvert.o -c $(SRC)/yomvert.F90

$(BUILD)/main_spnhsi.x: $(BUILD)/main_spnhsi.o $(BUILD)/abor1.o $(BUILD)/geometry_mod.o $(BUILD)/intdyn_mod.o $(BUILD)/mxmaop.o $(BUILD)/mxmaoptr.o $(BUILD)/mxptma.o $(BUILD)/mxture.o $(BUILD)/oml_mod.o $(BUILD)/parkind1.o $(BUILD)/reglatlon_field_mix.o $(BUILD)/sgemmx.o $(BUILD)/si_cccor.o $(BUILD)/si_mxptco.o $(BUILD)/sidd.o $(BUILD)/sigam.o $(BUILD)/sigam_nh.o $(BUILD)/simplico.o $(BUILD)/siptp.o $(BUILD)/siseve.o $(BUILD)/siskapi.o $(BUILD)/sitnu.o $(BUILD)/spcsidg_part0nh.o $(BUILD)/spcsidg_part1.o $(BUILD)/spcsidg_part2.o $(BUILD)/spnhsi.o $(BUILD)/tridia.o $(BUILD)/type_geometry.o $(BUILD)/type_spgeom.o $(BUILD)/util_geometry_mod.o $(BUILD)/util_reglatlon_field_mod.o $(BUILD)/util_tcsgleg_mod.o $(BUILD)/util_tcst_mod.o $(BUILD)/util_tcty_mod.o $(BUILD)/util_tcver_mod.o $(BUILD)/util_tdim_mod.o $(BUILD)/util_tdimv_mod.o $(BUILD)/util_tdyn_mod.o $(BUILD)/util_tdyna_mod.o $(BUILD)/util_teaerc_mod.o $(BUILD)/util_teaerc_tegen_mod.o $(BUILD)/util_tecmip_mod.o $(BUILD)/util_tedim_mod.o $(BUILD)/util_tegeo_mod.o $(BUILD)/util_tegsl_mod.o $(BUILD)/util_temmp_mod.o $(BUILD)/util_teozoc_mod.o $(BUILD)/util_tgem_mod.o $(BUILD)/util_tgflt_mod.o $(BUILD)/util_tgmvt_mod.o $(BUILD)/util_thwind_mod.o $(BUILD)/util_tlap_mod.o $(BUILD)/util_tlddh_mod.o $(BUILD)/util_tlep_mod.o $(BUILD)/util_tmp_mod.o $(BUILD)/util_tradghg_mod.o $(BUILD)/util_trcp_mod.o $(BUILD)/util_trip_mod.o $(BUILD)/util_tspgeom_mod.o $(BUILD)/util_tsta_mod.o $(BUILD)/util_tswe_mod.o $(BUILD)/util_ttnd_mod.o $(BUILD)/util_tvab_mod.o $(BUILD)/util_tvertical_geom_mod.o $(BUILD)/util_tveta_mod.o $(BUILD)/util_tvfe_mod.o $(BUILD)/util_txyb_mod.o $(BUILD)/util_txybder_mod.o $(BUILD)/verder.o $(BUILD)/verderfe.o $(BUILD)/verdisint.o $(BUILD)/verint.o $(BUILD)/verints.o $(BUILD)/xrd_getoptions.o $(BUILD)/xrd_unix_env.o $(BUILD)/yemdim.o $(BUILD)/yemgeo.o $(BUILD)/yemgsl.o $(BUILD)/yemlap.o $(BUILD)/yemmp.o $(BUILD)/yoeaerc.o $(BUILD)/yoeaerc_tegen.o $(BUILD)/yoecmip.o $(BUILD)/yoeozoc.o $(BUILD)/yoeradghg.o $(BUILD)/yomcst.o $(BUILD)/yomct0.o $(BUILD)/yomcver.o $(BUILD)/yomdata.o $(BUILD)/yomdim.o $(BUILD)/yomdimv.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o $(BUILD)/yomgem.o $(BUILD)/yomhook.o $(BUILD)/yomlap.o $(BUILD)/yomlddh.o $(BUILD)/yomleg.o $(BUILD)/yomlun.o $(BUILD)/yommp.o $(BUILD)/yommp0.o $(BUILD)/yomrip.o $(BUILD)/yomsta.o $(BUILD)/yomswe.o $(BUILD)/yomvert.o
	$(FC) -o $(BUILD)/main_spnhsi.x $(BUILD)/main_spnhsi.o $(BUILD)/abor1.o $(BUILD)/geometry_mod.o $(BUILD)/intdyn_mod.o $(BUILD)/mxmaop.o $(BUILD)/mxmaoptr.o $(BUILD)/mxptma.o $(BUILD)/mxture.o $(BUILD)/oml_mod.o $(BUILD)/parkind1.o $(BUILD)/reglatlon_field_mix.o $(BUILD)/sgemmx.o $(BUILD)/si_cccor.o $(BUILD)/si_mxptco.o $(BUILD)/sidd.o $(BUILD)/sigam.o $(BUILD)/sigam_nh.o $(BUILD)/simplico.o $(BUILD)/siptp.o $(BUILD)/siseve.o $(BUILD)/siskapi.o $(BUILD)/sitnu.o $(BUILD)/spcsidg_part0nh.o $(BUILD)/spcsidg_part1.o $(BUILD)/spcsidg_part2.o $(BUILD)/spnhsi.o $(BUILD)/tridia.o $(BUILD)/type_geometry.o $(BUILD)/type_spgeom.o $(BUILD)/util_geometry_mod.o $(BUILD)/util_reglatlon_field_mod.o $(BUILD)/util_tcsgleg_mod.o $(BUILD)/util_tcst_mod.o $(BUILD)/util_tcty_mod.o $(BUILD)/util_tcver_mod.o $(BUILD)/util_tdim_mod.o $(BUILD)/util_tdimv_mod.o $(BUILD)/util_tdyn_mod.o $(BUILD)/util_tdyna_mod.o $(BUILD)/util_teaerc_mod.o $(BUILD)/util_teaerc_tegen_mod.o $(BUILD)/util_tecmip_mod.o $(BUILD)/util_tedim_mod.o $(BUILD)/util_tegeo_mod.o $(BUILD)/util_tegsl_mod.o $(BUILD)/util_temmp_mod.o $(BUILD)/util_teozoc_mod.o $(BUILD)/util_tgem_mod.o $(BUILD)/util_tgflt_mod.o $(BUILD)/util_tgmvt_mod.o $(BUILD)/util_thwind_mod.o $(BUILD)/util_tlap_mod.o $(BUILD)/util_tlddh_mod.o $(BUILD)/util_tlep_mod.o $(BUILD)/util_tmp_mod.o $(BUILD)/util_tradghg_mod.o $(BUILD)/util_trcp_mod.o $(BUILD)/util_trip_mod.o $(BUILD)/util_tspgeom_mod.o $(BUILD)/util_tsta_mod.o $(BUILD)/util_tswe_mod.o $(BUILD)/util_ttnd_mod.o $(BUILD)/util_tvab_mod.o $(BUILD)/util_tvertical_geom_mod.o $(BUILD)/util_tveta_mod.o $(BUILD)/util_tvfe_mod.o $(BUILD)/util_txyb_mod.o $(BUILD)/util_txybder_mod.o $(BUILD)/verder.o $(BUILD)/verderfe.o $(BUILD)/verdisint.o $(BUILD)/verint.o $(BUILD)/verints.o $(BUILD)/xrd_getoptions.o $(BUILD)/xrd_unix_env.o $(BUILD)/yemdim.o $(BUILD)/yemgeo.o $(BUILD)/yemgsl.o $(BUILD)/yemlap.o $(BUILD)/yemmp.o $(BUILD)/yoeaerc.o $(BUILD)/yoeaerc_tegen.o $(BUILD)/yoecmip.o $(BUILD)/yoeozoc.o $(BUILD)/yoeradghg.o $(BUILD)/yomcst.o $(BUILD)/yomct0.o $(BUILD)/yomcver.o $(BUILD)/yomdata.o $(BUILD)/yomdim.o $(BUILD)/yomdimv.o $(BUILD)/yomdyn.o $(BUILD)/yomdyna.o $(BUILD)/yomgem.o $(BUILD)/yomhook.o $(BUILD)/yomlap.o $(BUILD)/yomlddh.o $(BUILD)/yomleg.o $(BUILD)/yomlun.o $(BUILD)/yommp.o $(BUILD)/yommp0.o $(BUILD)/yomrip.o $(BUILD)/yomsta.o $(BUILD)/yomswe.o $(BUILD)/yomvert.o $(LIBS)


exe: $(BUILD)/main_spnhsi.x

subclean:
	\rm -f $(BUILD)/abor1.o $(BUILD)/main_spnhsi.o $(BUILD)/mxmaop.o $(BUILD)/mxmaoptr.o $(BUILD)/mxptma.o $(BUILD)/mxture.o $(BUILD)/sgemmx.o $(BUILD)/si_cccor.o $(BUILD)/si_mxptco.o $(BUILD)/sidd.o $(BUILD)/sigam.o $(BUILD)/sigam_nh.o $(BUILD)/simplico.o $(BUILD)/siptp.o $(BUILD)/siseve.o $(BUILD)/siskapi.o $(BUILD)/sitnu.o $(BUILD)/spcsidg_part0nh.o $(BUILD)/spcsidg_part1.o $(BUILD)/spcsidg_part2.o $(BUILD)/spnhsi.o $(BUILD)/tridia.o $(BUILD)/verder.o $(BUILD)/verderfe.o $(BUILD)/verdisint.o $(BUILD)/verint.o $(BUILD)/verints.o

clean: 
	\rm -f $(BUILD)/*.o $(BUILD)/*.xml $(BUILD)/*.a $(BUILD)/*.x $(BUILD)/*.mod $(BUILD)/*.optrpt $(BUILD)/*.smod $(BUILD)/*.ok

tidy:
	\rm -f $(BUILD)/*.xml $(BUILD)/*.optrpt
