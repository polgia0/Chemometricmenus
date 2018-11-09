\name{chemo_toolbar}
\alias{chemo_toolbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Main toolbar}
\description{Main toolbar for Chemometric pacakage}
\usage{
chemo_toolbar()
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Riccardo Leardi and Gianmarco Polotti with contributions from Giorgio Marubini.Gruppo di Chemiometria (Divisione di Chimica Analitica della Societa' Chimica Italiana)}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    chemobar <- gtkMenuBar()
    DH_menu <- gtkMenu()
    DH_item <- gtkMenuItemNewWithMnemonic(label = "_Data Handling")
    DH_item$setSubmenu(DH_menu)
    chemobar$append(DH_item)
    DHwork_item <- gtkMenuItemNewWithMnemonic("_Workspace Management")
    gSignalConnect(DHwork_item, "activate", function(item) {
        DH_workspace_management()
    })
    DH_menu$append(DHwork_item)
    DHdatset_menu <- gtkMenu()
    DHman_item <- gtkMenuItemNewWithMnemonic("_Plot Magnify")
    gSignalConnect(DHman_item, "activate", function(item) {
        DH_magnify()
    })
    DH_menu$append(DHman_item)
    DHdatset_menu <- gtkMenu()
    DHdatset_item <- gtkMenuItemNewWithMnemonic(label = "Data_Set")
    DHdatset_item$setSubmenu(DHdatset_menu)
    DH_menu$append(DHdatset_item)
    DHrow_item <- gtkMenuItemNewWithMnemonic(label = "_Row")
    gSignalConnect(DHrow_item, "activate", function(item) {
        DH_dataset_row()
    })
    DHdatset_menu$append(DHrow_item)
    DHcol_item <- gtkMenuItemNewWithMnemonic(label = "_Column")
    gSignalConnect(DHcol_item, "activate", function(item) {
        DH_dataset_column()
    })
    DHdatset_menu$append(DHcol_item)
    DHload_menu <- gtkMenu()
    DHload_item <- gtkMenuItemNewWithMnemonic(label = "_Load")
    DHload_item$setSubmenu(DHload_menu)
    DH_menu$append(DHload_item)
    DHcsv_item <- gtkMenuItemNewWithMnemonic(label = "_CSV")
    gSignalConnect(DHcsv_item, "activate", function(item) {
        DH_load_csv()
    })
    DHload_menu$append(DHcsv_item)
    DHxls_item <- gtkMenuItemNewWithMnemonic(label = "_XLS")
    gSignalConnect(DHxls_item, "activate", function(item) {
        DH_load_xls()
    })
    DHload_menu$append(DHxls_item)
    DHtxt_item <- gtkMenuItemNewWithMnemonic(label = "_TXT")
    gSignalConnect(DHtxt_item, "activate", function(item) {
        DH_load_TXT()
    })
    DHload_menu$append(DHtxt_item)
    DHexp_menu <- gtkMenu()
    DHexp_item <- gtkMenuItemNewWithMnemonic(label = "_Export")
    DHexp_item$setSubmenu(DHexp_menu)
    DH_menu$append(DHexp_item)
    DHcsve_item <- gtkMenuItemNewWithMnemonic(label = "_CSV")
    gSignalConnect(DHcsve_item, "activate", function(item) {
        DH_export_CSV()
    })
    DHexp_menu$append(DHcsve_item)
    DHtxte_item <- gtkMenuItemNewWithMnemonic(label = "_TXT")
    gSignalConnect(DHtxte_item, "activate", function(item) {
        DH_export_TXT()
    })
    DHexp_menu$append(DHtxte_item)
    UN_menu <- gtkMenu()
    UN_item <- gtkMenuItemNewWithMnemonic(label = "_Univariate")
    UN_item$setSubmenu(UN_menu)
    chemobar$append(UN_item)
    UNsum_item <- gtkMenuItemNewWithMnemonic("_Summary")
    gSignalConnect(UNsum_item, "activate", function(item) {
        UN_summary()
    })
    UN_menu$append(UNsum_item)
    UNave_menu <- gtkMenu()
    UNave_item <- gtkMenuItemNewWithMnemonic(label = "_Average")
    UNave_item$setSubmenu(UNave_menu)
    UN_menu$append(UNave_item)
    UNari_item <- gtkMenuItemNewWithMnemonic(label = "_Arithmetic")
    gSignalConnect(UNari_item, "activate", function(item) {
        UN_average_arithmetic()
    })
    UNave_menu$append(UNari_item)
    UNgeo_item <- gtkMenuItemNewWithMnemonic(label = "_Geometric")
    gSignalConnect(UNgeo_item, "activate", function(item) {
        UN_average_geometric()
    })
    UNave_menu$append(UNgeo_item)
    UNrobu_menu <- gtkMenu()
    UNrobu_item <- gtkMenuItemNewWithMnemonic(label = "_Robust")
    UNrobu_item$setSubmenu(UNrobu_menu)
    UNave_menu$append(UNrobu_item)
    UNmedi_item <- gtkMenuItemNewWithMnemonic(label = "_Median")
    gSignalConnect(UNmedi_item, "activate", function(item) {
        UN_average_robust_median()
    })
    UNrobu_menu$append(UNmedi_item)
    UNdisp_menu <- gtkMenu()
    UNdisp_item <- gtkMenuItemNewWithMnemonic(label = "_Dispersion")
    UNdisp_item$setSubmenu(UNdisp_menu)
    UN_menu$append(UNdisp_item)
    UNsd_item <- gtkMenuItemNewWithMnemonic(label = "_Standard Deviation")
    gSignalConnect(UNsd_item, "activate", function(item) {
        UN_dispersion_sd()
    })
    UNdisp_menu$append(UNsd_item)
    UNv_item <- gtkMenuItemNewWithMnemonic(label = "_Variance")
    gSignalConnect(UNv_item, "activate", function(item) {
        UN_dispersion_var()
    })
    UNdisp_menu$append(UNv_item)
    UNrsd_item <- gtkMenuItemNewWithMnemonic(label = "_Coeff.of Variation(RSD)")
    gSignalConnect(UNrsd_item, "activate", function(item) {
        UN_dispersion_RSD()
    })
    UNdisp_menu$append(UNrsd_item)
    UNrobus_menu <- gtkMenu()
    UNrobus_item <- gtkMenuItemNewWithMnemonic(label = "_Robust")
    UNrobus_item$setSubmenu(UNrobus_menu)
    UNdisp_menu$append(UNrobus_item)
    UNiqr_item <- gtkMenuItemNewWithMnemonic(label = "_InterQuartile Range(IQR)")
    gSignalConnect(UNiqr_item, "activate", function(item) {
        UN_dispersion_robust_IQR.r()
    })
    UNrobus_menu$append(UNiqr_item)
    UNmad_item <- gtkMenuItemNewWithMnemonic(label = "_Median Absolute Deviation(MAD)")
    gSignalConnect(UNmad_item, "activate", function(item) {
        UN_dispersion_robust_MAD()
    })
    UNrobus_menu$append(UNmad_item)
    UNsmad_item <- gtkMenuItemNewWithMnemonic(label = "_MAD Standard Deviation(sMAD)")
    gSignalConnect(UNsmad_item, "activate", function(item) {
        UN_dispersion_robust_sMAD()
    })
    UNrobus_menu$append(UNsmad_item)
    UNgra_menu <- gtkMenu()
    UNgra_item <- gtkMenuItemNewWithMnemonic(label = "_Plots")
    UNgra_item$setSubmenu(UNgra_menu)
    UN_menu$append(UNgra_item)
    UNsp_item <- gtkMenuItemNewWithMnemonic(label = "_Strip (Scatter) Plot")
    gSignalConnect(UNsp_item, "activate", function(item) {
        UN_plot_stripchart()
    })
    UNgra_menu$append(UNsp_item)
    UNhi_item <- gtkMenuItemNewWithMnemonic(label = "_Histogram")
    gSignalConnect(UNhi_item, "activate", function(item) {
        UN_plot_hist()
    })
    UNgra_menu$append(UNhi_item)
    UNde_item <- gtkMenuItemNewWithMnemonic(label = "_Density")
    gSignalConnect(UNde_item, "activate", function(item) {
        UN_plot_density()
    })
    UNgra_menu$append(UNde_item)
    UNbox_item <- gtkMenuItemNewWithMnemonic(label = "_Boxplot")
    gSignalConnect(UNbox_item, "activate", function(item) {
        UN_plot_boxplot()
    })
    UNgra_menu$append(UNbox_item)
    UNeda_item <- gtkMenuItemNewWithMnemonic(label = "_Edaplot")
    gSignalConnect(UNeda_item, "activate", function(item) {
        UN_plot_edaplot()
    })
    UNgra_menu$append(UNeda_item)
    UNy_item <- gtkMenuItemNewWithMnemonic(label = "_Y")
    gSignalConnect(UNy_item, "activate", function(item) {
        UN_plot_Y()
    })
    UNgra_menu$append(UNy_item)
    chemo_window <- gtkWindow(type = "GTK_WINDOW_TOPLEVEL")
    chemo_vbox <- gtkVBox()
    chemo_window$add(chemo_vbox)
    chemo_vbox$packStart(chemobar, FALSE, FALSE)
    chemo_window$setTitle("chemometric Menubar")
    chemo_window$SetResizable(FALSE)
    chemo_window$Resize(750, 20)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
