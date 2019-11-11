cmake_minimum_required(VERSION 3.8)
project(MaMa VERSION 7.5.2 LANGUAGES Fortran C)

list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_SOURCE_DIR}/cmake)

#########################################
####### Checking for dependencies #######

find_package(X11 REQUIRED)
find_package(Motif REQUIRED)
find_package(Readline)

#########################################
###### Setting up targets

add_library(minig_x STATIC
        src/minig_x/cask.c
        src/minig_x/caskyn.c
        src/minig_x/minig_custom.c
        src/minig_x/miniga.c
        src/minig_x/minigb.c
        src/minig_x/minigc.c
        src/minig_x/qio_put_text.f
        src/minig_x/report_curs.c
        src/minig_x/set_focus.c
        src/minig_x/txtclr.f
        src/minig_x/util.c
        src/minig_x/write_no_advance.c
)

add_library(MaMa::minig_x ALIAS minig_x)

target_include_directories(minig_x
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include/minig_x>
    PRIVATE
        ${CMAKE_SOURCE_DIR}/src/minig_x
)

target_link_libraries(minig_x
    PUBLIC
        X11::X11
        ${Readline_LIBRARY})

if (READLINE_FOUND)
    target_compile_definitions(minig_x PRIVATE HAVE_GNU_READLINE=1)
endif()

add_library(gf2 STATIC
        src/gf2/addcomment.f
        src/gf2/adddelpk.f
        src/gf2/addwin.f
        src/gf2/ask.f
        src/gf2/askyn.f
        src/gf2/attshared2mama.f
        src/gf2/attspec2mama.c
        src/gf2/cask.f
        src/gf2/caskyn.f
        src/gf2/changecol.f
        src/gf2/chngmark.f
        src/gf2/cleanup.f
        src/gf2/comfil.f
        src/gf2/curse.f
        src/gf2/dofit.f
        src/gf2/dspfit.f
        src/gf2/dspma.f
        src/gf2/dspmkr.f
        src/gf2/dspsp.f
        src/gf2/energy.f
        src/gf2/environment.f
        src/gf2/eval.f
        src/gf2/ffin.f
        src/gf2/figega.f
        src/gf2/fitter.f
        src/gf2/fix_para.f
        src/gf2/fixorfree.f
        src/gf2/folding.f
        src/gf2/folding1.f
        src/gf2/folding2.f
        src/gf2/folding3.f
        src/gf2/folding4.f
        src/gf2/folding5.f
        src/gf2/folding6.f
        src/gf2/folding7.f
        src/gf2/folding8.f
        src/gf2/foldingy.f
        src/gf2/funcfit.f
        src/gf2/getmkrchnl.f
        src/gf2/gfexec.f
        src/gf2/gffin.f
        src/gf2/gfhelp.f
        src/gf2/gfinit.f
        src/gf2/gfset.f
        src/gf2/grabchar.c
        src/gf2/grax.f
        src/gf2/gridls.f
        src/gf2/inin.f
        src/gf2/inin_flg.f
        src/gf2/kelvin.f
        src/gf2/lineup.f
        src/gf2/matinv.f
        src/gf2/norway.f
        src/gf2/offspec2mama.c
        src/gf2/outlay.f
        src/gf2/para2num.f
        src/gf2/parset.f
        src/gf2/peakfind.f
        src/gf2/polyfit.f
        src/gf2/removepeak.f
        src/gf2/rewr.f
        src/gf2/setcts.f
        src/gf2/setext.f
        src/gf2/setmarker.f
        src/gf2/sumcts.f
        src/gf2/trax.f
        src/gf2/typeit.f
        src/gf2/uncorr.f
)

add_library(MaMa::gf2 ALIAS gf2)

target_include_directories(gf2
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
)

target_compile_options(gf2 PRIVATE -ffixed-line-length-132)


add_executable(mama app/gf2.f)

target_link_libraries(mama MaMa::gf2 MaMa::minig_x)

#########################################
###### Install instructions
install(TARGETS mama
        RUNTIME DESTINATION bin)