.onLoad<-function(libname, pkgname){
#	packageStartupMessage('Chemometric Menus Successfully  Loaded!')
}
 .onAttach<-function(libname, pkgname){
	# comand<-paste('WHERE /R',normalizePath(find.package('chemometricmenus'), winslash = "\\", mustWork = NA),'perlpath.RData',sep=' ')
	# suppressWarnings(perlpath<-system(comand,intern =TRUE))
	# if(is.null(attributes(perlpath)$status)){
		# load(paste(find.package('chemometricmenus'),'perlpath.RData',sep='/'))
		# packageStartupMessage('A perl interpreter is found ')
	# }else{
			# packageStartupMessage('Any perl interpreter was not found. Loading XLS file is not allowed.')
			# perlpath<-NULL
	# }
	# assign('perlpath',perlpath,envir=.GlobalEnv)
    chemo_toolbar()
}
file_path_sans_ext<-function(name){
	path<-str_split_fixed(name,'\\.',2)
	return(path[1])}
chemo_toolbar<-function(){
	if(!exists('previous.name',envir=.GlobalEnv))previous.name<-''
	# bar definition
	chemiobar<-gtkMenuBar()
	# Data Handling item
	DH_menu<-gtkMenu()
	DH_item<-gtkMenuItemNewWithMnemonic(label="_Data Handling")
	DH_item$setSubmenu(DH_menu)
	chemiobar$append(DH_item)
	DHload_menu<-gtkMenu()
	DHload_item<-gtkMenuItemNewWithMnemonic(label="_Load")
	DHload_item$setSubmenu(DHload_menu)
	DH_menu$append(DHload_item)
	DHcsv_item<-gtkMenuItemNewWithMnemonic(label="_CSV")
	gSignalConnect(DHcsv_item,"activate",function(item){DH_load_csv(previous.name)})
	DHload_menu$append(DHcsv_item)
	DHtxt_item<-gtkMenuItemNewWithMnemonic(label="_TXT")
	gSignalConnect(DHtxt_item,"activate",function(item){DH_load_txt(previous.name)})
	DHload_menu$append(DHtxt_item)
	DHxls_item<-gtkMenuItemNewWithMnemonic(label="_XLS/XLSX")
	gSignalConnect(DHxls_item,"activate",function(item){DH_load_xls(previous.name)})
	DHload_menu$append(DHxls_item)
	DHwork_item<-gtkMenuItemNewWithMnemonic("_Workspace Management")
	gSignalConnect(DHwork_item,"activate",function(item){DH_workspace_management(previous.name)})
	DH_menu$append(DHwork_item)
	DHdatset_menu<-gtkMenu()
	DHman_item<-gtkMenuItemNewWithMnemonic("_Plot Magnification")
	gSignalConnect(DHman_item,"activate",function(item){DH_magnify()})
	DH_menu$append(DHman_item)
	DHdatset_menu<-gtkMenu()
	DHdatset_item<-gtkMenuItemNewWithMnemonic(label="Data_Set")
	DHdatset_item$setSubmenu(DHdatset_menu)
	DH_menu$append(DHdatset_item)
	DHrow_item<-gtkMenuItemNewWithMnemonic(label="_Row")
	gSignalConnect(DHrow_item,"activate",function(item){DH_dataset_row(previous.name)})
	DHdatset_menu$append(DHrow_item)
	DHcol_item<-gtkMenuItemNewWithMnemonic(label="_Column")
	gSignalConnect(DHcol_item,"activate",function(item){DH_dataset_column(previous.name)})
	DHdatset_menu$append(DHcol_item)
	DHexp_menu<-gtkMenu()
	DHexp_item<-gtkMenuItemNewWithMnemonic(label="_Export")
	DHexp_item$setSubmenu(DHexp_menu)
	DH_menu$append(DHexp_item)
	DHcsve_item<-gtkMenuItemNewWithMnemonic(label="_CSV")
	gSignalConnect(DHcsve_item,"activate",function(item){DH_export_CSV(previous.name)})
	DHexp_menu$append(DHcsve_item)
	DHtxte_item<-gtkMenuItemNewWithMnemonic(label="_TXT")
	gSignalConnect(DHtxte_item,"activate",function(item){DH_export_TXT(previous.name)})
	DHexp_menu$append(DHtxte_item)
	# Univariate item
	UN_menu<-gtkMenu()
	UN_item<-gtkMenuItemNewWithMnemonic(label="_Univariate")
	UN_item$setSubmenu(UN_menu)
	chemiobar$append(UN_item)
	UNsum_item<-gtkMenuItemNewWithMnemonic("_Summary")
	gSignalConnect(UNsum_item,"activate",function(item){UN_summary(previous.name)})
	UN_menu$append(UNsum_item)
	UNave_menu<-gtkMenu()
	UNave_item<-gtkMenuItemNewWithMnemonic(label="_Average")
	UNave_item$setSubmenu(UNave_menu)
	UN_menu$append(UNave_item)
	UNari_item<-gtkMenuItemNewWithMnemonic(label="_Arithmetic")
	gSignalConnect(UNari_item,"activate",function(item){UN_average_arithmetic(previous.name)})
	UNave_menu$append(UNari_item)
	UNgeo_item<-gtkMenuItemNewWithMnemonic(label="_Geometric")
	gSignalConnect(UNgeo_item,"activate",function(item){UN_average_geometric(previous.name)})
	UNave_menu$append(UNgeo_item)
	UNrobu_menu<-gtkMenu()
	UNrobu_item<-gtkMenuItemNewWithMnemonic(label="_Robust")
	UNrobu_item$setSubmenu(UNrobu_menu)
	UNave_menu$append(UNrobu_item)
	UNmedi_item<-gtkMenuItemNewWithMnemonic(label="_Median")
	gSignalConnect(UNmedi_item,"activate",function(item){UN_average_robust_median(previous.name)})
	UNrobu_menu$append(UNmedi_item)
	UNdisp_menu<-gtkMenu()
	UNdisp_item<-gtkMenuItemNewWithMnemonic(label="_Dispersion")
	UNdisp_item$setSubmenu(UNdisp_menu)
	UN_menu$append(UNdisp_item)
	UNsd_item<-gtkMenuItemNewWithMnemonic(label="_Standard Deviation")
	gSignalConnect(UNsd_item,"activate",function(item){UN_dispersion_sd(previous.name)})
	UNdisp_menu$append(UNsd_item)
	UNv_item<-gtkMenuItemNewWithMnemonic(label="_Variance")
	gSignalConnect(UNv_item,"activate",function(item){UN_dispersion_var(previous.name)})
	UNdisp_menu$append(UNv_item)
	UNrsd_item<-gtkMenuItemNewWithMnemonic(label="_Coeff.of Variation(RSD)")
	gSignalConnect(UNrsd_item,"activate",function(item){UN_dispersion_RSD(previous.name)})
	UNdisp_menu$append(UNrsd_item)
	UNrobus_menu<-gtkMenu()
	UNrobus_item<-gtkMenuItemNewWithMnemonic(label="_Robust")
	UNrobus_item$setSubmenu(UNrobus_menu)
	UNdisp_menu$append(UNrobus_item)
	UNiqr_item<-gtkMenuItemNewWithMnemonic(label="_InterQuartile Range(IQR)")
	gSignalConnect(UNiqr_item,"activate",function(item){UN_dispersion_robust_IRQ(previous.name)})
	UNrobus_menu$append(UNiqr_item)
	UNmad_item<-gtkMenuItemNewWithMnemonic(label="_Median Absolute Deviation(MAD)")
	gSignalConnect(UNmad_item,"activate",function(item){UN_dispersion_robust_MAD(previous.name)})
	UNrobus_menu$append(UNmad_item)
	UNsmad_item<-gtkMenuItemNewWithMnemonic(label="_MAD Standard Deviation(sMAD)")
	gSignalConnect(UNsmad_item,"activate",function(item){UN_dispersion_robust_sMAD(previous.name)})
	UNrobus_menu$append(UNsmad_item)
	UNgra_menu<-gtkMenu()
	UNgra_item<-gtkMenuItemNewWithMnemonic(label="_Plots")
	UNgra_item$setSubmenu(UNgra_menu)
	UN_menu$append(UNgra_item)
	UNsp_item<-gtkMenuItemNewWithMnemonic(label="_Strip (Scatter) Plot")
	gSignalConnect(UNsp_item,"activate",function(item){UN_plot_stripchart(previous.name)})
	UNgra_menu$append(UNsp_item)
	UNhi_item<-gtkMenuItemNewWithMnemonic(label="_Histogram")
	gSignalConnect(UNhi_item,"activate",function(item){UN_plot_hist(previous.name)})
	UNgra_menu$append(UNhi_item)
	UNde_item<-gtkMenuItemNewWithMnemonic(label="_Density")
	gSignalConnect(UNde_item,"activate",function(item){UN_plot_density(previous.name)})
	UNgra_menu$append(UNde_item)
	UNbox_item<-gtkMenuItemNewWithMnemonic(label="_Boxplot")
	gSignalConnect(UNbox_item,"activate",function(item){UN_plot_boxplot(previous.name)})
	UNgra_menu$append(UNbox_item)
	UNeda_item<-gtkMenuItemNewWithMnemonic(label="_Edaplot")
	gSignalConnect(UNeda_item,"activate",function(item){UN_plot_edaplot(previous.name)})
	UNgra_menu$append(UNeda_item)
	UNy_item<-gtkMenuItemNewWithMnemonic(label="_Y")
	gSignalConnect(UNy_item,"activate",function(item){UN_plot_Y(previous.name)})
	UNgra_menu$append(UNy_item)
	# Bivariate item
	BI_menu<-gtkMenu()
	BI_item<-gtkMenuItemNewWithMnemonic(label="_Bivariate")
	BI_item$setSubmenu(BI_menu)
	chemiobar$append(BI_item)
	BIcov_item<-gtkMenuItemNewWithMnemonic("_Covariance")
	gSignalConnect(BIcov_item,"activate",function(item){BI_covariance(previous.name)})
	BI_menu$append(BIcov_item)
	BIcor_item<-gtkMenuItemNewWithMnemonic("_Correlation")
	gSignalConnect(BIcor_item,"activate",function(item){BI_correlation(previous.name)})
	BI_menu$append(BIcor_item)
	BIrob_menu<-gtkMenu()
	BIrob_item<-gtkMenuItemNewWithMnemonic(label="_Robust")
	BIrob_item$setSubmenu(BIrob_menu)
	BI_menu$append(BIrob_item)
	BImcd_item<-gtkMenuItemNewWithMnemonic(label="_MCD")
	gSignalConnect(BImcd_item,"activate",function(item){BI_robust_MCD(previous.name)})
	BIrob_menu$append(BImcd_item)
	BIspe_menu<-gtkMenu()
	BIspe_item<-gtkMenuItemNewWithMnemonic(label="_Special")
	BIspe_item$setSubmenu(BIspe_menu)
	BI_menu$append(BIspe_item)
	BIsper_item<-gtkMenuItemNewWithMnemonic(label="_Spearman")
	gSignalConnect(BIsper_item,"activate",function(item){BI_spearman(previous.name)})
	BIspe_menu$append(BIsper_item)
	BIken_item<-gtkMenuItemNewWithMnemonic(label="_Kendall")
	gSignalConnect(BIken_item,"activate",function(item){BI_kendall(previous.name)})
	BIspe_menu$append(BIken_item)
	BIgra_menu<-gtkMenu()
	BIgra_item<-gtkMenuItemNewWithMnemonic(label="_Plots")
	BIgra_item$setSubmenu(BIgra_menu)
	BI_menu$append(BIgra_item)
	BIpar_item<-gtkMenuItemNewWithMnemonic(label="_Pairs")
	gSignalConnect(BIpar_item,"activate",function(item){BI_plot_pairs(previous.name)})
	BIgra_menu$append(BIpar_item)
	BIxy_item<-gtkMenuItemNewWithMnemonic(label="_X vs. Y")
	gSignalConnect(BIxy_item,"activate",function(item){BI_plot_XY(previous.name)})
	BIgra_menu$append(BIxy_item)
	BIcorm_item<-gtkMenuItemNewWithMnemonic(label="_Corr. Matrix")
	gSignalConnect(BIcorm_item,"activate",function(item){BI_plot_cor(previous.name)})
	BIgra_menu$append(BIcorm_item)
	# Transformation item
	TR_menu<-gtkMenu()
	TR_item<-gtkMenuItemNewWithMnemonic(label="_Transformations")
	TR_item$setSubmenu(TR_menu)
	chemiobar$append(TR_item)
	TRmatr_menu<-gtkMenu()
	TRmatr_item<-gtkMenuItemNewWithMnemonic(label="Matrix by _row")
	TRmatr_item$setSubmenu(TRmatr_menu)
	TR_menu$append(TRmatr_item)
	TRsumr_item<-gtkMenuItemNewWithMnemonic(label="_Sum 100")
	gSignalConnect(TRsumr_item,"activate",function(item){TR_row_sum100(previous.name)})
	TRmatr_menu$append(TRsumr_item)
	TRautr_item<-gtkMenuItemNewWithMnemonic(label="_Autoscale (snv)")
	gSignalConnect(TRautr_item,"activate",function(item){TR_row_autoscale(previous.name)})
	TRmatr_menu$append(TRautr_item)
	TR1dr_item<-gtkMenuItemNewWithMnemonic(label="_First Derivative")
	gSignalConnect(TR1dr_item,"activate",function(item){TR_row_der_first(previous.name)})
	TRmatr_menu$append(TR1dr_item)
	TR2dr_item<-gtkMenuItemNewWithMnemonic(label="_Second Derivative")
	gSignalConnect(TR2dr_item,"activate",function(item){TR_row_der_second(previous.name)})
	TRmatr_menu$append(TR2dr_item)
	TRmatc_menu<-gtkMenu()
	TRmatc_item<-gtkMenuItemNewWithMnemonic(label="Matrix by _column")
	TRmatc_item$setSubmenu(TRmatc_menu)
	TR_menu$append(TRmatc_item)
	TRsumc_item<-gtkMenuItemNewWithMnemonic(label="_Sum 100")
	gSignalConnect(TRsumc_item,"activate",function(item){TR_column_sum100(previous.name)})
	TRmatc_menu$append(TRsumc_item)
	TRmax_item<-gtkMenuItemNewWithMnemonic(label="_Max 100")
	gSignalConnect(TRmax_item,"activate",function(item){TR_column_max100(previous.name)})
	TRmatc_menu$append(TRmax_item)
	TRlen_item<-gtkMenuItemNewWithMnemonic(label="_Length 1")
	gSignalConnect(TRlen_item,"activate",function(item){TR_column_length1(previous.name)})
	TRmatc_menu$append(TRlen_item)
	TR01_item<-gtkMenuItemNewWithMnemonic(label="_O - +1")
	gSignalConnect(TR01_item,"activate",function(item){TR_column_01(previous.name)})
	TRmatc_menu$append(TR01_item)
	TR11_item<-gtkMenuItemNewWithMnemonic(label="_-1 - +1")
	gSignalConnect(TR11_item,"activate",function(item){TR_column_11(previous.name)})
	TRmatc_menu$append(TR11_item)
	TRcen_item<-gtkMenuItemNewWithMnemonic(label="_Centering")
	gSignalConnect(TRcen_item,"activate",function(item){TR_column_centering(previous.name)})
	TRmatc_menu$append(TRcen_item)
	TRsca_item<-gtkMenuItemNewWithMnemonic(label="_Scaling")
	gSignalConnect(TRsca_item,"activate",function(item){TR_column_scaling(previous.name)})
	TRmatc_menu$append(TRsca_item)
	TRautc_item<-gtkMenuItemNewWithMnemonic(label="_Autoscaling")
	gSignalConnect(TRautc_item,"activate",function(item){TR_column_autoscale(previous.name)})
	TRmatc_menu$append(TRautc_item)
	TRpow_item<-gtkMenuItemNewWithMnemonic(label="_Power (Box-Cox)")
	gSignalConnect(TRpow_item,"activate",function(item){TR_column_boxcox(previous.name)})
	TRmatc_menu$append(TRpow_item)
	TRlog_item<-gtkMenuItemNewWithMnemonic(label="_Logit")
	gSignalConnect(TRlog_item,"activate",function(item){TR_column_logit(previous.name)})
	TRmatc_menu$append(TRlog_item)
	TRrcs_item<-gtkMenuItemNewWithMnemonic(label="_Robust Centering & Scaling")
	gSignalConnect(TRrcs_item,"activate",function(item){TR_column_rubust_centerscale(previous.name)})
	TRmatc_menu$append(TRrcs_item)
	TR1dc_item<-gtkMenuItemNewWithMnemonic(label="_First Derivative")
	gSignalConnect(TR1dc_item,"activate",function(item){TR_column_der_first(previous.name)})
	TRmatc_menu$append(TR1dc_item)
	TR2dc_item<-gtkMenuItemNewWithMnemonic(label="_Second Derivative")
	gSignalConnect(TR2dc_item,"activate",function(item){TR_column_der_second(previous.name)})
	TRmatc_menu$append(TR2dc_item)
	TRmatg_menu<-gtkMenu()
	TRmatg_item<-gtkMenuItemNewWithMnemonic(label="Matrix _global")
	TRmatg_item$setSubmenu(TRmatg_menu)
	TR_menu$append(TRmatg_item)
	TRcen_item<-gtkMenuItemNewWithMnemonic(label="_Centering")
	gSignalConnect(TRcen_item,"activate",function(item){TR_global_centering(previous.name)})
	TRmatg_menu$append(TRcen_item)
	TRcenlg_item<-gtkMenuItemNewWithMnemonic(label="_Centered Logratio")
	gSignalConnect(TRcenlg_item,"activate",function(item){TR_global_centerlogit(previous.name)})
	TRmatg_menu$append(TRcenlg_item)
	TRisolg_item<-gtkMenuItemNewWithMnemonic(label="_Isometric Logratio")
	gSignalConnect(TRisolg_item,"activate",function(item){TR_global_isologit(previous.name)})
	TRmatg_menu$append(TRisolg_item)
	# Distance item
	DST_menu<-gtkMenu()
	DST_item<-gtkMenuItemNewWithMnemonic(label="_Distance")
	DST_item$setSubmenu(DST_menu)
	chemiobar$append(DST_item)
	DSTeuc_item<-gtkMenuItemNewWithMnemonic("_Euclidean")
	gSignalConnect(DSTeuc_item,"activate",function(item){DST_euclidean(previous.name)})
	DST_menu$append(DSTeuc_item)
	DSTman_item<-gtkMenuItemNewWithMnemonic("_Manhattan")
	gSignalConnect(DSTman_item,"activate",function(item){DST_manhattan(previous.name)})
	DST_menu$append(DSTman_item)
	DSTmax_item<-gtkMenuItemNewWithMnemonic("_Maximum")
	gSignalConnect(DSTmax_item,"activate",function(item){DST_maximum(previous.name)})
	DST_menu$append(DSTmax_item)
	DSTmah_item<-gtkMenuItemNewWithMnemonic("_Mahalanobis")
	gSignalConnect(DSTmah_item,"activate",function(item){DST_mahalanobis(previous.name)})
	DST_menu$append(DSTmah_item)
	DSTmcd_item<-gtkMenuItemNewWithMnemonic("_Mahalanobis MCD")
	gSignalConnect(DSTmcd_item,"activate",function(item){DST_mahalanobis_MCD(previous.name)})
	DST_menu$append(DSTmcd_item)
	# PCA item
	PCA_menu<-gtkMenu()
	PCA_item<-gtkMenuItemNewWithMnemonic(label="_PCA")
	PCA_item$setSubmenu(PCA_menu)
	chemiobar$append(PCA_item)
	PCAmod_menu<-gtkMenu()
	PCAmod_item<-gtkMenuItemNewWithMnemonic("_Model Computation")
	PCAmod_item$setSubmenu(PCAmod_menu)
	PCA_menu$append(PCAmod_item)
	PCApca_item<-gtkMenuItemNewWithMnemonic("_PCA")
	gSignalConnect(PCApca_item,"activate",function(item){PCA_model_PCA(previous.name)})
	PCAmod_menu$append(PCApca_item)
	PCAvar_item<-gtkMenuItemNewWithMnemonic("_Varimax")
	gSignalConnect(PCAvar_item,"activate",function(item){PCA_model_varimax(previous.name)})
	PCAmod_menu$append(PCAvar_item)
	PCApcs_item<-gtkMenuItemNewWithMnemonic(label="_Number PCs Determination(CV)")
	gSignalConnect(PCApcs_item,"activate",function(item){PCA_number_pcs_determination()})
	PCA_menu$append(PCApcs_item)
	PCAmdr_item<-gtkMenuItemNewWithMnemonic(label="_Missing Data Reconstruction")
	gSignalConnect(PCAmdr_item,"activate",function(item){PCA_data_reconstruction(previous.name)})
	PCA_menu$append(PCAmdr_item)
	PCAext_item<-gtkMenuItemNewWithMnemonic("Extract")
	gSignalConnect(PCAext_item,"activate",function(item){PCA_extract()})
	PCA_menu$append(PCAext_item)
	PCAplot_menu<-gtkMenu()
	PCAplot_item<-gtkMenuItemNewWithMnemonic(label="_Plots")
	PCAplot_item$setSubmenu(PCAplot_menu)
	PCA_menu$append(PCAplot_item)
	PCAvar_item<-gtkMenuItemNewWithMnemonic(label="_Variance Plot")
	gSignalConnect(PCAvar_item,"activate",function(item){PCA_variance_plot()})
	PCAplot_menu$append(PCAvar_item)
	PCAcvar_item<-gtkMenuItemNewWithMnemonic(label="_Cumulative Var. Plot")
	gSignalConnect(PCAcvar_item,"activate",function(item){PCA_cumulative_var_plot()})
	PCAplot_menu$append(PCAcvar_item)
	PCAevar_item<-gtkMenuItemNewWithMnemonic(label="Variance _expl. by each Variable")
	gSignalConnect(PCAevar_item,"activate",function(item){PCA_explained_variance_variable()})
	PCAplot_menu$append(PCAevar_item)
	PCAlods_item<-gtkMenuItemNewWithMnemonic(label="_Loading Plot (Scatter)")
	gSignalConnect(PCAlods_item,"activate",function(item){PCA_loading_plot_scatter()})
	PCAplot_menu$append(PCAlods_item)
	PCAlodb_item<-gtkMenuItemNewWithMnemonic(label="Loading _Plot (Bar)")
	gSignalConnect(PCAlodb_item,"activate",function(item){PCA_loading_plot_bar()})
	PCAplot_menu$append(PCAlodb_item)
	PCAsco_item<-gtkMenuItemNewWithMnemonic(label="_Score Plot")
	gSignalConnect(PCAsco_item,"activate",function(item){PCA_score_plot()})
	PCAplot_menu$append(PCAsco_item)
	PCAbi_item<-gtkMenuItemNewWithMnemonic(label="_Biplot")
	gSignalConnect(PCAbi_item,"activate",function(item){PCA_biplot()})
	PCAplot_menu$append(PCAbi_item)
	PCAdiag_menu<-gtkMenu()
	PCAdiag_item<-gtkMenuItemNewWithMnemonic(label="_Diagnostic")
	PCAdiag_item$setSubmenu(PCAdiag_menu)
	PCA_menu$append(PCAdiag_item)
	PCAtq_item<-gtkMenuItemNewWithMnemonic(label="_T^2 and Q")
	gSignalConnect(PCAtq_item,"activate",function(item){PCA_diagnostic_plot_t2q()})
	PCAdiag_menu$append(PCAtq_item)
	PCAtvsq_item<-gtkMenuItemNewWithMnemonic(label="T^2 vs. _Q")
	gSignalConnect(PCAtvsq_item,"activate",function(item){PCA_diagnostic_plot_t2vsq()})
	PCAdiag_menu$append(PCAtvsq_item)
	PCAcon_item<-gtkMenuItemNewWithMnemonic(label="_Contribution Plots")
	gSignalConnect(PCAcon_item,"activate",function(item){PCA_diagnostic_cont_plot()})
	PCAdiag_menu$append(PCAcon_item)
	PCAexd_menu<-gtkMenu()
	PCAexd_item<-gtkMenuItemNewWithMnemonic(label="_External Data Set")
	PCAexd_item$setSubmenu(PCAexd_menu)
	PCA_menu$append(PCAexd_item)
	PCAeds_item<-gtkMenuItemNewWithMnemonic(label="_Projection on the training set")
	gSignalConnect(PCAeds_item,"activate",function(item){PCA_projection_training_set()})
	PCAexd_menu$append(PCAeds_item)
	PCAtqd_item<-gtkMenuItemNewWithMnemonic(label="_T^2 vs.Q")
	gSignalConnect(PCAtqd_item,"activate",function(item){PCA_t2vsq_Dataset(previous.name)})
	PCAexd_menu$append(PCAtqd_item)
	PCAconds_item<-gtkMenuItemNewWithMnemonic(label="_Contribution Plots")
	gSignalConnect(PCAconds_item,"activate",function(item){PCA_cont_plot_Dataset()})
	PCAexd_menu$append(PCAconds_item)
	# 3W PCA item
	W3_menu<-gtkMenu()
	W3_item<-gtkMenuItemNewWithMnemonic(label="_3W-PCA")
	W3_item$setSubmenu(W3_menu)
	chemiobar$append(W3_item)
	W3mod_item<-gtkMenuItemNewWithMnemonic("_Model")
	gSignalConnect(W3mod_item,"activate",function(item){W3_model(previous.name)})
	W3_menu$append(W3mod_item)
	W3ext_item<-gtkMenuItemNewWithMnemonic("_Extract")
	gSignalConnect(W3ext_item,"activate",function(item){W3_extract()})
	W3_menu$append(W3ext_item)
	W3plot_menu<-gtkMenu()
	W3plot_item<-gtkMenuItemNewWithMnemonic(label="_Plots")
	W3plot_item$setSubmenu(W3plot_menu)
	W3_menu$append(W3plot_item)
	W3obj_item<-gtkMenuItemNewWithMnemonic(label="_Objects")
	gSignalConnect(W3obj_item,"activate",function(item){W3_plot_objects()})
	W3plot_menu$append(W3obj_item)
	W3con_item<-gtkMenuItemNewWithMnemonic(label="_Conditions")
	gSignalConnect(W3con_item,"activate",function(item){W3_plot_conditions()})
	W3plot_menu$append(W3con_item)
	W3var_item<-gtkMenuItemNewWithMnemonic(label="_Variables")
	gSignalConnect(W3var_item,"activate",function(item){W3_plot_variables()})
	W3plot_menu$append(W3var_item)
	W3tri_item<-gtkMenuItemNewWithMnemonic(label="_Triplot")
	gSignalConnect(W3tri_item,"activate",function(item){W3_plot_triplot()})
	W3plot_menu$append(W3tri_item)
	W3robj_item<-gtkMenuItemNewWithMnemonic(label="_RMSE Objects")
	gSignalConnect(W3robj_item,"activate",function(item){W3_plot_rmse_objects()})
	W3plot_menu$append(W3robj_item)
	W3rcon_item<-gtkMenuItemNewWithMnemonic(label="R_MSE Conditions")
	gSignalConnect(W3rcon_item,"activate",function(item){W3_plot_rmse_conditions()})
	W3plot_menu$append(W3rcon_item)
	W3rvar_item<-gtkMenuItemNewWithMnemonic(label="RM_SE Variables")
	gSignalConnect(W3rvar_item,"activate",function(item){W3_plot_rmse_variables()})
	W3plot_menu$append(W3rvar_item)
	# DOE item
	DOE_menu<-gtkMenu()
	DOE_item<-gtkMenuItemNewWithMnemonic(label="MLR-_DOE")
	DOE_item$setSubmenu(DOE_menu)
	chemiobar$append(DOE_item)
	DOEdopt_item<-gtkMenuItemNewWithMnemonic("_D-Optimal design")
	gSignalConnect(DOEdopt_item,"activate",function(item){DOE_doptimal(previous.name)})
	DOE_menu$append(DOEdopt_item)
	DOEdoptadd_item<-gtkMenuItemNewWithMnemonic("_D-Optimal addition")
	gSignalConnect(DOEdoptadd_item,"activate",function(item){DOE_doptadd(previous.name)})
	DOE_menu$append(DOEdoptadd_item)
	DOEmod_item<-gtkMenuItemNewWithMnemonic(label="_Model Computation")
	gSignalConnect(DOEmod_item,"activate",function(item){DOE_model_computation(previous.name)})
	DOE_menu$append(DOEmod_item)
	DOEplot_menu<-gtkMenu()
	DOEplot_item<-gtkMenuItemNewWithMnemonic(label="_Plots")
	DOEplot_item$setSubmenu(DOEplot_menu)
	DOE_menu$append(DOEplot_item)
	DOEcoe_item<-gtkMenuItemNewWithMnemonic("_Coefficients")
	gSignalConnect(DOEcoe_item,"activate",function(item){DOE_coefficients()})
	DOEplot_menu$append(DOEcoe_item)
	DOEexp_item<-gtkMenuItemNewWithMnemonic("_Experimental vs. Fitted")
	gSignalConnect(DOEexp_item,"activate",function(item){DOE_experimental_fitted()})
	DOEplot_menu$append(DOEexp_item)
	DOEecv_item<-gtkMenuItemNewWithMnemonic("Experimental vs. _CV Predicted")
	gSignalConnect(DOEecv_item,"activate",function(item){DOE_experimental_predicted()})
	DOEplot_menu$append(DOEecv_item)
	DOEresexp_item<-gtkMenuItemNewWithMnemonic("_Residuals in Fitting")
	gSignalConnect(DOEresexp_item,"activate",function(item){DOE_residuals_fitting()})
	DOEplot_menu$append(DOEresexp_item)
	DOEresexp_item<-gtkMenuItemNewWithMnemonic("R_esiduals in CV")
	gSignalConnect(DOEresexp_item,"activate",function(item){DOE_CVresiduals_experimental()})
	DOEplot_menu$append(DOEresexp_item)
	DOEext_item<-gtkMenuItemNewWithMnemonic("_Extract")
	gSignalConnect(DOEext_item,"activate",function(item){DOE_extract()})
	DOE_menu$append(DOEext_item)
	DOEpre_item<-gtkMenuItemNewWithMnemonic("_Prediction")
	gSignalConnect(DOEpre_item,"activate",function(item){DOE_prediction(previous.name)})
	DOE_menu$append(DOEpre_item)
	DOElev_item<-gtkMenuItemNewWithMnemonic("_Leverage Surface")
	gSignalConnect(DOElev_item,"activate",function(item){DOE_leverage_surface()})
	DOE_menu$append(DOElev_item)
	DOEsur_item<-gtkMenuItemNewWithMnemonic("Response _Surface")
	gSignalConnect(DOEsur_item,"activate",function(item){DOE_response_surface()})
	DOE_menu$append(DOEsur_item)
	# Calibration item
	CAL_menu<-gtkMenu()
	CAL_item<-gtkMenuItemNewWithMnemonic(label="_Calibration")
	CAL_item$setSubmenu(CAL_menu)
	chemiobar$append(CAL_item)
	CALmod_menu<-gtkMenu()
	CALmod_item<-gtkMenuItemNewWithMnemonic(label="_Model Computation")
	CALmod_item$setSubmenu(CALmod_menu)
	CAL_menu$append(CALmod_item)
	CALpcr_item<-gtkMenuItemNewWithMnemonic(label="_PCR")
	gSignalConnect(CALpcr_item,"activate",function(item){CAL_model_computation_PCR(previous.name)})
	CALmod_menu$append(CALpcr_item)
	CALpls1_item<-gtkMenuItemNewWithMnemonic(label="_PLS1")
	gSignalConnect(CALpls1_item,"activate",function(item){CAL_model_computation_PLS1(previous.name)})
	CALmod_menu$append(CALpls1_item)
	CALpls2_item<-gtkMenuItemNewWithMnemonic(label="_PLS2")
	gSignalConnect(CALpls2_item,"activate",function(item){CAL_model_computation_PLS2(previous.name)})
	CALmod_menu$append(CALpls2_item)
	CALext_item<-gtkMenuItemNewWithMnemonic("_Extract")
	gSignalConnect(CALext_item,"activate",function(item){CAL_extract()})
	CAL_menu$append(CALext_item)
	CALbi_item<-gtkMenuItemNewWithMnemonic("_BiPlot")
	gSignalConnect(CALbi_item,"activate",function(item){CAL_biplot()})
	CAL_menu$append(CALbi_item)
	CALlod_item<-gtkMenuItemNewWithMnemonic("_x-Loadings")
	gSignalConnect(CALlod_item,"activate",function(item){CAL_xloadings()})
	CAL_menu$append(CALlod_item)
	CALsco_item<-gtkMenuItemNewWithMnemonic("_Scores")
	gSignalConnect(CALsco_item,"activate",function(item){CAL_scores()})
	CAL_menu$append(CALsco_item)
	CALcoe_item<-gtkMenuItemNewWithMnemonic("_Coefficients")
	gSignalConnect(CALcoe_item,"activate",function(item){CAL_coefficients()})
	CAL_menu$append(CALcoe_item)
	CALres_item<-gtkMenuItemNewWithMnemonic("_Residuals")
	gSignalConnect(CALres_item,"activate",function(item){CAL_residuals()})
	CAL_menu$append(CALres_item)
	CALexp_item<-gtkMenuItemNewWithMnemonic("_Experimental vs.Calculated")
	gSignalConnect(CALexp_item,"activate",function(item){CAL_experimental_calculated()})
	CAL_menu$append(CALexp_item)
	CALpre_item<-gtkMenuItemNewWithMnemonic("_Prediction")
	gSignalConnect(CALpre_item,"activate",function(item){CAL_prediction(previous.name)})
	CAL_menu$append(CALpre_item)
	# Classification item
	CL_menu<-gtkMenu()
	CL_item<-gtkMenuItemNewWithMnemonic(label="_Classification")
	CL_item$setSubmenu(CL_menu)
	chemiobar$append(CL_item)
	CLmet_menu<-gtkMenu()
	CLmet_item<-gtkMenuItemNewWithMnemonic(label="_Method")
	CLmet_item$setSubmenu(CLmet_menu)
	CL_menu$append(CLmet_item)
	CLlda_item<-gtkMenuItemNewWithMnemonic(label="_LDA")
	gSignalConnect(CLlda_item,"activate",function(item){CL_method_LDA(previous.name)})
	CLmet_menu$append(CLlda_item)
	CLqda_item<-gtkMenuItemNewWithMnemonic(label="_QDA")
	gSignalConnect(CLqda_item,"activate",function(item){CL_method_QDA(previous.name)})
	CLmet_menu$append(CLqda_item)
	CLext_item<-gtkMenuItemNewWithMnemonic("_Extract")
	gSignalConnect(CLext_item,"activate",function(item){CL_extract()})
	CL_menu$append(CLext_item)
	CLpre_menu<-gtkMenu()
	CLpre_item<-gtkMenuItemNewWithMnemonic(label="_Prediction")
	CLpre_item$setSubmenu(CLpre_menu)
	CL_menu$append(CLpre_item)
	CLplda_menu<-gtkMenu()
	CLplda_item<-gtkMenuItemNewWithMnemonic(label="_LDA")
	gSignalConnect(CLplda_item,"activate",function(item){CL_prediction_LDA(previous.name)})
	CLpre_menu$append(CLplda_item)
	CLpqda_menu<-gtkMenu()
	CLpqda_item<-gtkMenuItemNewWithMnemonic(label="_QDA")
	gSignalConnect(CLpqda_item,"activate",function(item){CL_prediction_QDA(previous.name)})
	CLpre_menu$append(CLpqda_item)
	CLplot_menu<-gtkMenu()
	CLplot_item<-gtkMenuItemNewWithMnemonic(label="_Plots")
	CLplot_item$setSubmenu(CLplot_menu)
	CL_menu$append(CLplot_item)
	CLmod_menu<-gtkMenu()
	CLmod_item<-gtkMenuItemNewWithMnemonic(label="_Model")
	CLmod_item$setSubmenu(CLmod_menu)
	CLplot_menu$append(CLmod_item)
	CLppre_menu<-gtkMenu()
	CLppre_item<-gtkMenuItemNewWithMnemonic(label="_Prediction")
	CLppre_item$setSubmenu(CLppre_menu)
	CLplot_menu$append(CLppre_item)
	CLmah_item<-gtkMenuItemNewWithMnemonic(label="_CV Mahalanobis Distance")
	gSignalConnect(CLmah_item,"activate",function(item){CL_plot_mahalanobis()})
	CLmod_menu$append(CLmah_item)
	CLmahc_item<-gtkMenuItemNewWithMnemonic(label="_CV Mahalanobis Distance (category)")
	gSignalConnect(CLmahc_item,"activate",function(item){CL_plot_mahalanobis_cat()})
	CLmod_menu$append(CLmahc_item)
	CLmaho_item<-gtkMenuItemNewWithMnemonic(label="_CV Mahalanobis Distance (object)")
	gSignalConnect(CLmaho_item,"activate",function(item){CL_plot_mahalanobis_obj()})
	CLmod_menu$append(CLmaho_item)
	CLmahp_item<-gtkMenuItemNewWithMnemonic(label="_Mahalanobis Distance")
	gSignalConnect(CLmahp_item,"activate",function(item){CL_pre_mahalanobis()})
	CLppre_menu$append(CLmahp_item)
	CLmahcp_item<-gtkMenuItemNewWithMnemonic(label="Mahalanobis Distance (_category)")
	gSignalConnect(CLmahcp_item,"activate",function(item){CL_pre_mahalanobis_cat()})
	CLppre_menu$append(CLmahcp_item)
	CLmahop_item<-gtkMenuItemNewWithMnemonic(label="Mahalanobis Distance (_object)")
	gSignalConnect(CLmahop_item,"activate",function(item){CL_pre_mahalanobis_obj()})
	CLppre_menu$append(CLmahop_item)
	# build bar
	chemio_window<-gtkWindow(type='GTK_WINDOW_TOPLEVEL')
	chemio_vbox<-gtkVBox()
	chemio_window$add(chemio_vbox)
	chemio_vbox$packStart(chemiobar,FALSE,FALSE)
	chemio_window$setTitle('Chemiometric Menubar')
	chemio_window$SetResizable(FALSE)
	chemio_window$Resize(750,20)
}
DH_dataset_column<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if((typeof(M)=='list')|(typeof(M)=='double')){
			ans<-inpboxcr4(c('Column Number','Select:','Delete','Extract','Row Names','Copy'),c(as.character(1:ncol(M)),'name'))
			if(as.numeric(ans[[1]])<=ncol(M)){
				if(!is.null(ans)){
					nc<-ans[[1]]
					ex.col<-unlist(M[,nc])
					if(ans[[2]]){
						M<-as.data.frame(M[,-nc])
					}
					if(ans[[3]]){
						M<-as.data.frame(M[,-nc])
						assign('ex.col',ex.col,envir=.GlobalEnv)
						print('Column is copied in variable: ex.col')
					}
					if(ans[[4]]){
						M<-as.data.frame(M[,-nc])
						M<-as.data.frame(M)
						rownames(M)<-ex.col
					}
					if(ans[[5]]){
						assign('ex.col',ex.col,envir=.GlobalEnv)
						print('Column is copied in variable: ex.col')
					}
				assign(name,M,envir=.GlobalEnv)
				}
			}else{
				if(ans[[3]]|ans[[4]]|ans[[5]]){
					assign('vname',rownames(M),envir=.GlobalEnv)
					print('Row Names are copied in vector: vname')
				}else{
					row.names(M)<-NULL
					assign(name,M,envir=.GlobalEnv)
					print('Row Names are deleted')
				}
			}
		}else{tk_messageBox(type=c("ok"),message='Only Matrix for this Function!',caption="Input Error")}
	}
}
DH_dataset_row<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if((typeof(M)=='list')|(typeof(M)=='double')){
			ans<-inpboxcr4(c('Row Number','Select:','Delete','Extract','Column Names','Copy'),c(as.character(1:nrow(M)),'name'))
			if(as.numeric(ans[[1]])<=nrow(M)){
				if(!is.null(ans)){
					nr<-ans[[1]]
					ex.row<-unlist(M[nr,])
					if(ans[[2]]){
						M<-as.data.frame(M[-nr,])
					}
					if(ans[[3]]){
						M<-as.data.frame(M[-nr,])
						assign('ex.row',ex.row,envir=.GlobalEnv)
						print('Row is copied in variable: ex.row')
					}
					if(ans[[4]]){
						M<-as.data.frame(M[-nr,])
						M<-as.data.frame(M)
						colnames(M)<-ex.row
					}
					if(ans[[5]]){
						assign('ex.row',ex.row,envir=.GlobalEnv)
						print('Row is copied in variable: ex.row')
					}
					assign(name,M,envir=.GlobalEnv)
				}
			}else{
				if(ans[[3]]|ans[[4]]|ans[[5]]){
					assign('vname',names(M),envir=.GlobalEnv)
					print('Column Names are copied in vector: vname')
				}else{
					names(M)<-as.character(1:ncol(M))
					assign(name,M,envir=.GlobalEnv)
					print('Row Names are deleted')
				}
			}
		}else{tk_messageBox(type=c("ok"),message='Only Matrix for this Function!',caption="Input Error")} 
	}
}
DH_export_CSV<-function(previous.name=''){
	ans<-inpboxe4k2(c('*Matrix Name','*Field Separator','*Decimal Separator','*Missing Data','Header','Row Names'),c(previous.name,';',',','NA','TRUE','FALSE'))
	if(!is.null(ans)){
	    name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			if((as.logical(ans[[5]]))&(as.logical(ans[[6]]))){
				mt<-names(M)
				if(is.null(mt))mt<-as.character(1:ncol(M))
				mt<-matrix(c(' ',mt),1,ncol(M)+1)
				write.table(mt,paste(name,'.csv',sep=''),sep=as.character(ans[[2]]),quote=TRUE,row.names=FALSE,col.names=FALSE)
				write.table(M,paste(name,'.csv',sep=''),sep=as.character(ans[[2]]),quote=TRUE,append=TRUE,dec=as.character(ans[[3]]),na=as.character(ans[[4]]),row.names=TRUE,col.names=FALSE);rm(mt)
			}
			if((as.logical(ans[[5]]))&(!as.logical(ans[[6]]))){
				write.table(M,paste(name,'.csv',sep=''),sep=as.character(ans[[2]]),quote=TRUE,dec=as.character(ans[[3]]),na=as.character(ans[[4]]),row.names=FALSE,col.names=TRUE)
			}
			if((!as.logical(ans[[5]]))&(as.logical(ans[[6]]))){
				write.table(M,paste(name,'.csv',sep=''),sep=as.character(ans[[2]]),quote=TRUE,dec=as.character(ans[[3]]),na=as.character(ans[[4]]),row.names=TRUE,col.names=FALSE)
			}
			if((!as.logical(ans[[5]]))&(!as.logical(ans[[6]]))){
				write.table(M,paste(name,'.csv',sep=''),sep=as.character(ans[[2]]),quote=TRUE,dec=as.character(ans[[3]]),na=as.character(ans[[4]]),row.names=as.logical(ans[[6]]),col.names=as.logical(ans[[5]]))
			}
			assign('previous.name',name,envir=.GlobalEnv)
		}else{tk_messageBox(type=c("ok"),message='The Variable does not exist!',caption="Input Error")}
	}
}
DH_export_TXT<-function(previous.name=''){
	ans<-inpboxe3k2(c('*Matrix Name','*Decimal Separator','*Missing Data','Header','Row Names'),c(previous.name,';',',','NA','TRUE','FALSE'))
	if(!is.null(ans)){
	    name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			if((as.logical(ans[[4]]))&(as.logical(ans[[5]]))){
				mt<-dimnames(M)[[2]]
				if(is.null(mt))mt<-as.character(1:ncol(M))
				mt<-matrix(c(' ',mt),1,ncol(M)+1)
				write.table(mt,paste(name,'.txt',sep=''),sep="\t",quote=TRUE,row.names=FALSE,col.names=FALSE)
				write.table(M,paste(name,'.txt',sep=''),sep="\t",quote=TRUE,append=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=TRUE,col.names=FALSE);rm(mt)
			}
			if((as.logical(ans[[4]]))&(!as.logical(ans[[5]]))){
				write.table(M,paste(name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=FALSE,col.names=TRUE)
			}
			if((!as.logical(ans[[4]]))&(as.logical(ans[[5]]))){
				write.table(M,paste(name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=TRUE,col.names=FALSE)
			}
			if((!as.logical(ans[[4]]))&(!as.logical(ans[[5]]))){
				write.table(M,paste(name,'.txt',sep=''),sep="\t",quote=TRUE,dec=as.character(ans[[2]]),na=as.character(ans[[3]]),row.names=as.logical(ans[[6]]),col.names=as.logical(ans[[5]]))
			}
			assign('previous.name',name,envir=.GlobalEnv)
		}else{tk_messageBox(type=c("ok"),message='The Variable does not exist!',caption="Input Error")}
	}
}
DH_load_csv<-function(previous.name=''){
	nome_file<-file.choose(new = FALSE)
	ans<-inpboxe6k2(c('*Matrix Name','*Field Separator','*Decimal Separator','*Missing Data','*Skip Top Rows','*Skip Left Columns','Header','Row Names'),
					c(str_replace_all(file_path_sans_ext(basename(nome_file))," ",""),';',',','NA','0','0','TRUE','FALSE'))
	if(!is.null(ans)){
		name<-ans[[1]]
		oyn<-'yes'
		if(exists(name,envir=.GlobalEnv)){
			oyn<-tk_messageBox(type="yesno",message='Variable Exists. Overwrite ?')
		}
		if(oyn=='yes'){
			if(length(name)!=0){
				M<-read.csv2(nome_file,header=as.logical(ans[[7]]),sep=ans[[2]],quote="\"",dec=ans[[3]],na.strings=ans[[4]],stringsAsFactors=FALSE,blank.lines.skip=TRUE)
				if(as.numeric(ans[[5]])!=0)M<-M[-(1:as.numeric(ans[[5]])),]
				if(as.numeric(ans[[6]])!=0)M<-M[,-(1:as.numeric(ans[[6]]))]
				if(as.logical(ans[[8]])){
					rownames(M)<-M[,1]
					M<-M[,-1]
				}
			}
			assign(name,M,envir=.GlobalEnv)
			assign('previous.name',name,envir=.GlobalEnv)
			nr_<-nrow(M)
			nc_<-ncol(M)
			print(paste(nr_*nc_,' Data loaded: ',nr_,' Rows & ',nc_,' Columns',sep=''),quote=FALSE)
			if(nc_>=700)print(paste('Suspicius Data Loading: ',nc_,' variables loaded!',sep=''),quote=FALSE)
		}
	}
}
DH_load_txt<-function(previous.name=''){
	nome_file<-file.choose(new = FALSE)
	ans<-inpboxe4k2(c('* Matrix Name','* Decimal Separator','* Skip Top Rows','* Skip Left Columns','Header','Row Names'),
				    c(str_replace_all(file_path_sans_ext(basename(nome_file))," ",""),',','0','0','TRUE','FALSE'))
	if(!is.null(ans)){
		name<-ans[[1]]
		oyn<-'yes'
		if(exists(name,envir=.GlobalEnv)){
			oyn<-tk_messageBox(type="yesno",message='Variable Exists.Overwrite ?')
		}
		if(oyn=='yes'){
			M<-read.delim(nome_file,header=as.logical(ans[[5]]),sep="\t",quote="\"",dec=as.character(ans[[2]]),fill=TRUE)
			if(as.numeric(ans[[3]])!=0)M<-M[-(1:as.numeric(ans[[4]])),]
			if(as.numeric(ans[[4]])!=0)M<-M[,-(1:as.numeric(ans[[4]]))]
			if(as.logical(ans[[6]])){
				rownames(M)<-M[,1]
				M<-M[,-1]
			}
			assign(name,M,envir=.GlobalEnv)
			assign('previous.name',name,envir=.GlobalEnv)
			nr_<-nrow(M)
			nc_<-ncol(M)
			print(paste(nr_*nc_,' Data loaded: ',nr_,' Rows & ',nc_,' Columns',sep=''),quote=FALSE)
			if(nc_>=700)print(paste('Suspicius Data Loading: ',nc_,' variables loaded!',sep=''),quote=FALSE)
		}
	}
}
DH_load_xls<-function(previous.name=''){
    perlpath<-'C:/strawberry/perl/bin/perl.exe'
	if(!is.null(perlpath)){
		nome_file<-file.choose(new = FALSE)
		ans<-inpboxe4k2(c('* Matrix Name','* Sheet n.','* Skip Top Rows','* Skip Left Columns','Header','Row Names'),
					c(str_replace_all(file_path_sans_ext(basename(nome_file))," ",""),'1','0','0','TRUE','FALSE'))
		if(!is.null(ans)){
			name<-ans[[1]]
			oyn<-'yes'
			if(exists(name,envir=.GlobalEnv)){
				oyn<-tk_messageBox(type="yesno",message='Variable Exists. Overwrite ?')
			}
			if(oyn=='yes'){
				if(length(name)!=0){
					M<-read.xls(nome_file,sheet=as.numeric(ans[[2]]),skip=as.numeric(ans[[3]]),perl=perlpath,header=as.logical(ans[[5]]))
					if(as.numeric(ans[[4]])!=0)M<-M[,-(1:as.numeric(ans[[4]]))]
					if(as.logical(ans[[6]])){
						rownames(M)<-M[,1]
						M<-M[,-1]
					}
				}
				assign(name,M,envir=.GlobalEnv)
				assign('previous.name',name,envir=.GlobalEnv)
				nr_<-nrow(M)
				nc_<-ncol(M)
				print(paste(nr_*nc_,' Data loaded: ',nr_,' Rows & ',nc_,' Columns',sep=''),quote=FALSE)
				if(nc_>=700)print(paste('Suspicius Data Loading: ',nc_,' variables loaded!',sep=''),quote=FALSE)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message="Apparently you have not perl installed.You cannot use this menu.",caption="Input Error")
	}
}
DH_magnify<-function(){
	if(dev.cur()>=2){
		print('Close the Plot Window after you have done !',quote=FALSE)
		zm()
	}else{
		tk_messageBox(type=c("ok"),message="A plot must be drawn first !",caption="Input Error")
	}
}
DH_workspace_management<-function(previous.name=''){
	workspace_management(previous.name)
}
UN_average_arithmetic<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(strsplit(name,'\\[')[[1]][1],envir=.GlobalEnv)|
		  exists(strsplit(name,'\\$')[[1]][1],envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			if(as.logical(ans[[2]])){
				if(is.numeric(unlist(M))){
					var.mean<-mean(unlist(M),na.rm=TRUE)
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
			if(as.logical(ans[[3]])){
				if(is.numeric(t(M))){
					var.mean<-apply(M,1,mean,na.rm=TRUE)
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
			if(as.logical(ans[[4]])){
				if(is.numeric(M)){
					var.mean<-apply(M,2,mean,na.rm=TRUE)
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
		}
	}
}
UN_average_geometric<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(strsplit(name,'\\[')[[1]][1],envir=.GlobalEnv)|
		  exists(strsplit(name,'\\$')[[1]][1],envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			if(as.logical(ans[[2]])){
				if(is.numeric(unlist(M))){
					var.mean<-prod(as.vector(M))^(1/length(na.omit(unlist(M))))
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
			if(as.logical(ans[[3]])){
				if(is.numeric(t(M))){
					var.mean<-apply(M,1,prod,na.rm=TRUE)
					var.mean<-var.mean^(1/apply(M,1,f<-function(s){length(na.omit(s))}))	
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
			if(as.logical(ans[[4]])){
				if(is.numeric(M)){
					var.mean<-apply(M,2,prod,na.rm=TRUE)
					var.mean<-var.mean^(1/apply(M,2,f<-function(s){length(na.omit(s))}))	
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
		}
	}
}
UN_average_robust_huber<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(strsplit(name,'\\[')[[1]][1],envir=.GlobalEnv)|
		  exists(strsplit(name,'\\$')[[1]][1],envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			if(as.logical(ans[[2]])){
				if(is.numeric(unlist(M))){
					var.mean<-huber(na.omit(unlist(M)))$mu
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
			if(as.logical(ans[[3]])){
				if(is.numeric(t(M))){
					var.mean<-apply(M,1,f<-function(s){huber(na.omit(s))$mu})
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
			if(as.logical(ans[[4]])){
				if(is.numeric(M)){
					var.mean<-apply(M,2,f<-function(s){huber(na.omit(s))$mu})
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
		}
	}
}
UN_average_robust_median<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(strsplit(name,'\\[')[[1]][1],envir=.GlobalEnv)|
		  exists(strsplit(name,'\\$')[[1]][1],envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			if(as.logical(ans[[2]])){
				if(is.numeric(unlist(M))){
					var.mean<-median(unlist(M),na.rm=TRUE)
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
			if(as.logical(ans[[3]])){
				if(is.numeric(t(M))){
					var.mean<-apply(M,1,median,na.rm=TRUE)
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
			if(as.logical(ans[[4]])){
				if(is.numeric(M)){
					var.mean<-apply(M,2,median,na.rm=TRUE)
					assign('var.mean',var.mean,envir=.GlobalEnv)
					print('The value is saved in: var.mean',quote=FALSE)
					print(var.mean)
				}else{
					print('The matrix contains alphanumeric data: this operation is not allowed',quote=FALSE)
				}
			}
		}
	}
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			M<-as.matrix(M)
			if(as.logical(ans[[2]]))var.mean<-median(as.vector(M))
			if(as.logical(ans[[3]]))var.mean<-apply(M,1,median)
			if(as.logical(ans[[4]]))var.mean<-apply(M,2,median)
			assign('var.mean',var.mean,envir=.GlobalEnv)
			print('The value is saved in: var.mean')
			print(var.mean)
		}
	}
}
UN_dispersion_robust_IRQ<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			M<-as.matrix(M)
			if(as.logical(ans[[2]]))var.dispersion<-IQR(as.vector(M))
			if(as.logical(ans[[3]]))var.dispersion<-apply(M,1,IQR)
			if(as.logical(ans[[4]]))var.dispersion<-apply(M,2,IQR)
			assign('var.dispersion',var.dispersion,envir=.GlobalEnv)
			print('The value is saved in: var.dispersion',quote=FALSE)
			print(var.dispersion)
		}
	}
}
UN_dispersion_robust_MAD<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			M<-as.matrix(M)
			if(as.logical(ans[[2]]))var.dispersion<-mad(as.vector(M))
			if(as.logical(ans[[3]]))var.dispersion<-apply(M,1,mad)
			if(as.logical(ans[[4]]))var.dispersion<-apply(M,2,mad)
			assign('var.dispersion',var.dispersion,envir=.GlobalEnv)
			print('The value is saved in: var.dispersion',quote=FALSE)
			print(var.dispersion)
		}
	}
}
UN_dispersion_robust_sMAD<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			M<-as.matrix(M)
			if(as.logical(ans[[2]]))var.dispersion<-100*mad(as.vector(M))/median(as.vector(M))
			if(as.logical(ans[[3]])){
				var.dispersion<-apply(M,1,mad)
				var.mean<-apply(M,1,median)
				var.dispersion<-100*var.dispersion/var.mean
			}
			if(as.logical(ans[[4]])){
				var.dispersion<-apply(M,2,mad)
				var.mean<-apply(M,2,median)
				var.dispersion<-100*var.dispersion/var.mean
			}
			assign('var.dispersion',var.dispersion,envir=.GlobalEnv)
			print('The value is saved in: var.dispersion',quote=FALSE)
			print(var.dispersion)
		}
	}
}
UN_dispersion_RSD<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			M<-as.matrix(M)
			if(as.logical(ans[[2]]))var.dispersion<-100*sd(as.vector(M))/mean(as.vector(M))
			if(as.logical(ans[[3]])){
				var.dispersion<-apply(M,1,sd)
				var.mean<-apply(M,1,mean)
				var.dispersion<-100*var.dispersion/var.mean
			}
			if(as.logical(ans[[4]])){
				var.dispersion<-apply(M,2,sd)
				var.mean<-apply(M,2,mean)
				var.dispersion<-100*var.dispersion/var.mean
			}
			assign('var.dispersion',var.dispersion,envir=.GlobalEnv)
			print('The value is saved in: var.dispersion',quote=FALSE)
			print(var.dispersion)
		}
	}
}
UN_dispersion_sd<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			M<-as.matrix(M)
			if(as.logical(ans[[2]]))var.dispersion<-sd(as.vector(M))
			if(as.logical(ans[[3]]))var.dispersion<-apply(M,1,sd)
			if(as.logical(ans[[4]]))var.dispersion<-apply(M,2,sd)
			assign('var.dispersion',var.dispersion,envir=.GlobalEnv)
			print('The value is saved in: var.dispersion',quote=FALSE)
			print(var.dispersion)
		}
	}
}
UN_dispersion_var<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		if(exists(name,envir=.GlobalEnv)){
			M<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			M<-as.matrix(M)
			if(as.logical(ans[[2]]))var.dispersion<-var(as.vector(M))
			if(as.logical(ans[[3]]))var.dispersion<-apply(M,1,var)
			if(as.logical(ans[[4]]))var.dispersion<-apply(M,2,var)
			assign('var.dispersion',var.dispersion,envir=.GlobalEnv)
			print('The value is saved in: var.dispersion',quote=FALSE)
			print(var.dispersion)
		}
	}
}
UN_plot_boxplot<-function(previous.name=''){
	ans<-inpboxe1('*Vector',previous.name)
	name<-ans[[1]]
	if(!is.null(ans)){
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.vector(M)
		if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			nr<-nrow(M)
			nc<-ncol(M)
		if((nc==1)|(nr==1)){
			boxplot(M,xlab='',ylab='',main='Box Plot')
			grid()
		}else{tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption="Input Error")}
	}
}
UN_plot_density<-function(previous.name=''){
	ans<-inpboxe1('*Vector',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.vector(M)
		if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			nr<-nrow(M)
			nc<-ncol(M)
		if((nc==1)|(nr==1)){
			plot(density(M),xlab='',ylab='',main='Density Plot')
			grid()
		}else{tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption="Input Error")}
	}
}
UN_plot_edaplot<-function(previous.name=''){
	ans<-inpboxe1('*Vector',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			nr<-nrow(M)
			nc<-ncol(M)
		if((nc==1)|(nr==1)){
			edaplot(M,H.freq=FALSE,P.axes=FALSE,P.xlab='',P.ylab='',P.main='Eda Plot')
			axis(1, at = NULL, labels = TRUE)
			grid()
		}else{tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption="Input Error")}
	}
}
UN_plot_hist<-function(previous.name=''){
	ans<-inpboxe1('*Vector',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.vector(M)
		if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			nr<-nrow(M)
			nc<-ncol(M)
		if((nc==1)|(nr==1)){
			hist(M,xlab='',main='Histogram')
			box()
		}else{tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption="Input Error")}
	}
}
UN_plot_stripchart<-function(previous.name=''){
	ans<-inpboxe1('*Vector',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M,1,length(M))
		if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
			M<-na.omit(M)
			nr<-nrow(M)
			nc<-ncol(M)
		if((nc==1)|(nr==1)){
			stripchart(M,method="overplot",jitter=0.1,offset=1/3,vertical=FALSE,ylab='',xlab='',main='Strip Plot')
			grid()
		}else{tk_messageBox(type=c("ok"),message='The Variable must be a single vector !',caption="Input Error")}
	}
}
UN_plot_Y<-function(previous.name=''){
	ans<-inpboxe3k2(c('*Matrix Name (e.g., A[,1])','Label Vector (e.g., A[,1])','Color Vector (e.g., A[,1])','Line','Point'),c(previous.name,'None','None','FALSE','TRUE'))
	if(!is.null(ans)){
		tex<-NULL;grade<-NULL
		name<-ans[[1]]
		yvar<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		xymatrix<-data.frame(x=1:length(yvar),y=yvar,f=rep(0,length(yvar)),g=unlist(dovc(rep(0,length(yvar)))))
		if(ans[[2]]!='None')xymatrix$f<-as.character(givemat(ans[[2]],nl=length(yvar)))
		if(ans[[3]]!='None')xymatrix$g<-as.character(unlist(dovc(givemat(ans[[3]],nl=length(yvar)))))
		if(sum(is.na(xymatrix))!=0)print('>>NA found and ignored<<',quote=FALSE)
		xymatrix<-na.omit(xymatrix)
		plot(c(min(xymatrix[,1]),max(xymatrix[,1])),c(min(xymatrix[,2]),max(xymatrix[,2])),'n',xlab='Object Index',ylab='Variable')
		grid()
		if(as.logical(ans[[4]]))lines(xymatrix[,1],xymatrix[,2],col=as.character(xymatrix[,4]))
		if(as.logical(ans[[5]])&(ans[[2]]=='None'))points(xymatrix[,1],xymatrix[,2],col=as.character(xymatrix[,4]),pch=16)
		if(ans[[2]]!='None')text(xymatrix[,1],xymatrix[,2],labels=xymatrix[,3],col=as.character(xymatrix[,4]))
	}
}
UN_summary<-function(previous.name=''){
	ans<-inpboxer3(c('*Matrix','','All','Row wise','Column wise'),previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if(sum(is.na(M))!=0)print('>>NA found and ignored<<',quote=FALSE)
		if(as.logical(ans[[2]])){
			if(is.numeric(t(M))){
				print.table(summary(unlist(M)))	
			}else{
				print('Matrix not numeric: operation not allowed',quote=FALSE)
			}
		}
		if(as.logical(ans[[3]])){
			if(is.numeric(t(M))){
				print.table(summary(t(M)))
			}else{
				print('Matrix not numeric: operation not allowed',quote=FALSE)
			}
		}
		if(as.logical(ans[[4]]))print.table(summary(M))
	}
}
BI_correlation<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			nr<-nrow(M)
			nc<-ncol(M)
			if((nc>=2)&(nr>=2)){
				var.cor<-cor(M)
				assign('var.cor',var.cor,envir=.GlobalEnv)
				print('The value is saved in: var.cor')
				print(round(as.dist(var.cor),3))
			}else{
				tk_messageBox(type=c('ok'),message='Variable must have dimension greater than 2!',caption='Input Error')
			}
		}
	}
}
BI_covariance<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			nr<-nrow(M)
			nc<-ncol(M)
			if((nc>=2)&(nr>=2)){
				var.cov<-cov(M)
				assign('var.cov',var.cov,envir=.GlobalEnv)
				print('The value is saved in: var.cov')
				print(round(as.dist(var.cov),3))
			}else{
				tk_messageBox(type=c('ok'),message='Variable must have dimension greater than 2!',caption='Input Error')
			}
		}
	}
}
BI_kendall<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			nr<-nrow(M)
			nc<-ncol(M)
			if((nc>=2)&(nr>=2)){
				var.ken<-cor(M,method='kendall')
				assign('var.ken',var.ken,envir=.GlobalEnv)
				print('The value is saved in: var.ken')
				print(round(as.dist(var.ken),3))
			}else{
				tk_messageBox(type=c('ok'),message='Variable must have dimension greater than 2!',caption='Input Error')
			}
		}
	}
}
BI_spearman<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{		
			nr<-nrow(M)
			nc<-ncol(M)
			if((nc>=2)&(nr>=2)){
				var.spear<-cor(M,method='spearman')
				assign('var.spear',var.spear,envir=.GlobalEnv)
				print('The value is saved in: var.spear')
				print(round(as.dist(var.spear),3))
			}else{
				tk_messageBox(type=c('ok'),message='Variable must have dimension greater than 2!',caption='Input Error')
			}
		}
	}
}
BI_robust_MCD<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			nr<-nrow(M)
			nc<-ncol(M)
			if((nc>=2)&(nr>=2)){
				var.mcd<-covMcd(M,alpha=0.75,cor=TRUE)$cor
				assign('var.mcd',var.mcd,envir=.GlobalEnv)
				print('The value is saved in: var.mcd')
				print(var.mcd)
			}else{
				tk_messageBox(type=c('ok'),message='Variable must have dimension greater than 2!',caption='Input Error')
			}
		}
	}
}
BI_plot_cor<-function(previous.name=''){
	BI_correlation(previous.name)
	get('var.cor',envir=.GlobalEnv)
	if(exists(var.cor)){
		n<-ncol(var.cor)
		C<-matrix(rep(0,n*n),n,n)
		for(i in 1:n){for(j in 1:n){C[i,n-(j-1)]=var.cor[i,j]}}
		vc<-redblue(256)
		vc<-rev(vc)
		nvc<-round(min(var.cor)*20)
		image(1:n,1:n,C,col=vc[(20+nvc):256],xlab='Variable Index',ylab='Variable Index',
		main='Blue-Negative : White-Null : Red-Positive',yaxt='n',xaxt='n')
		axis(at=seq(1,n),labels=seq(1,n),side=1,cex.axis=0.6)
		axis(at=seq(1,n),labels=rev(seq(1,n)),side=2,cex.axis=0.6)
		if(n<10){
			for(i in 1:(n-1)){
				abline(h=i+0.5,lty=1,col='black')
				abline(v=i+0.5,lty=1,col='black')
			}
		}
	}
}
BI_plot_pairs<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			nr<-nrow(M)
			nc<-ncol(M)
			if(((nc>=2)&(nc<=7))|((nr>=2)&(nr<=7))){
				pairs(M)
			}else{
				tk_messageBox(type=c('ok'),message='Variable must have dimension >2 and <7!',caption='Input Error')
			}
		}
	}
}
BI_plot_XY<-function(previous.name=''){
	ans<-inpboxe4k2(c('*Variable on x-axis (e.g., A[,1])','*Variable on y-axis (e.g., A[,2])','Label Vector','Color Vector','Line','Points'),
	c(previous.name,'','None','None','FALSE','TRUE'))
	if(!is.null(ans)){
		x<-givemat(ans[[1]])
		n<-length(x)
		y<-givemat(ans[[2]])
		if(length(x)==length(y)){
			xymatrix<-data.frame(x=x,y=y,lb=rep('',n),g=unlist(dovc(rep(1,n))))
			if(ans[[3]]!='None')xymatrix$lb<-givemat(ans[[3]])
			if(ans[[4]]!='None'){
				g<-givemat(ans[[4]])
				if(is.factor(g))g<-as.character(g)				
				xymatrix$g<-unlist(dovc(g))
			}
			if(nrow(xymatrix)>1){
				if(sum(is.na(xymatrix))!=0)print('>>NA found and ignored<<')
				xymatrix<-na.omit(xymatrix)
				plot(xymatrix$x,xymatrix$y,xlim=c(min(xymatrix$x),max(xymatrix$x)),ylim=c(min(xymatrix$y),max(xymatrix$y)),'n',xlab='',ylab='')
				if(as.logical(ans[[5]]))lines(xymatrix$x,xymatrix$y,col=xymatrix$g)
				if(as.logical(ans[[6]])&(ans[[3]]=='None'))points(xymatrix$x,xymatrix$y,col=xymatrix$g,pch=16)
				if(ans[[3]]!='None')text(xymatrix$x,xymatrix$y,labels=xymatrix$lb,col=as.character(xymatrix$g))
			}
		}else{
			tk_messageBox(type=c('ok'),message='Vectors must have the same length',caption='Input Error')
		}
	}
}
TR_column_01<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.vector(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			vmax<-apply(M,2,max)
			vmin<-apply(M,2,min)
			D<-vmax-vmin
			var.trans<-M
			for(i in 1:ncol(M)){
				var.trans[,i]<-(M[,i]-vmin[i])/D[i]
			}
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_11<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.vector(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			vmax<-apply(M,2,max)
			vmin<-apply(M,2,min)
			D<-vmax-vmin
			var.trans<-M
			for(i in 1:ncol(M)){
				var.trans[,i]<-2*(M[,i]-vmin[i])/D[i]-1
			}
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_autoscale<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-scale(M,center=TRUE,scale=TRUE)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_boxcox<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			p<-boxcoxfit(M)$lambda # optimal exponent of the power
			var.trans<-bcPower(M,p,jacobian.adjusted=FALSE)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_centering<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.vector(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-scale(M,center=TRUE,scale=FALSE)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_der_first<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-diff(M,lag=1,differences=1)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_der_second<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-diff(M,lag=1,differences=2)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_length1<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.vector(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			sv<-apply(M^2,2,sum)
			var.trans<-M
			for(i in 1:ncol(M)){
				var.trans[,i]<-M[,i]/sqrt(sv[i])
			}
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_logit<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-M
			no<-0
			for(i in 1:ncol(M)){
				for(j in 1:nrow(M)){
					if((M[j,i]>0)&(M[j,i]<=1)){
						var.trans[j,i]<-0.5*log(M[j,i]/(1-M[j,i]))
					}else{
						tk_messageBox(type=c('ok'),message='Vector must have all elements between 0 and 1',caption='Input Error')
						no<-1
						break
					}
				}
				if(no==1)break
			}
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_max100<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.vector(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			sv<-apply(M,2,max)
			var.trans<-M
			for(i in 1:ncol(M)){
				var.trans[,i]<-M[,i]/sv[i]*100
			}
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_rubust_centerscale<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-scale(M,center=apply(M,2,median),scale=apply(M,2,mad))
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_scaling<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-scale(M,center=FALSE,scale=TRUE)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_column_sum100<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
				print('>>NA found: remove them before evaluation<<')
		}else{
			sv<-apply(M,2,sum)
			var.trans<-M
			for(i in 1:ncol(M)){
				var.trans[,i]<-M[,i]/sv[i]*100
			}
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_global_centering<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-centering(M,col.first=TRUE)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_global_centerlogit<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-clr(M)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_global_isologit<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			var.trans<-ilr(M)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_row_autoscale<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			M<-t(M)
			var.trans<-scale(M,center=TRUE,scale=TRUE)
			var.trans<-t(var.trans)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_row_der_first<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			M<-t(M)
			var.trans<-diff(M,lag=1,differences=1)
			var.trans<-t(var.trans)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_row_der_second<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			M<-t(M)
			var.trans<-diff(M,lag=1,differences=2)
			var.trans<-t(var.trans)
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
TR_row_sum100<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			sv<-apply(M,1,sum)
			var.trans<-M
			for(i in 1:nrow(M)){
				var.trans[i,]<-M[i,]/sv[i]*100
			}
			assign('var.trans',var.trans,envir=.GlobalEnv)
			print('The value is saved in: var.trans')
			print(var.trans)
		}
	}
}
DST_euclidean<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			if((nrow(M)!=1)&(ncol(M)!=1)){
				var.dist<-as.matrix(dist(M,method="euclidian"))
				assign('var.dist',var.dist,envir=.GlobalEnv)
				print('The value is saved in: var.dist')
				print(var.dist)
			}else{
				tk_messageBox(type=c("ok"),message='Variable must have both dimensions greater than 2!',caption="Input Error")
			}
		}
	}
}
DST_mahalanobis<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			if((nrow(M)!=1)&(ncol(M)!=1)){
				v.mean<-apply(M,2,mean)
				var.dist<-as.matrix(sqrt(mahalanobis(M,v.mean,cov(M))))
				assign('var.dist',var.dist,envir=.GlobalEnv)
				print('The value is saved in: var.dist')
				print(var.dist)
			}else{
				tk_messageBox(type=c("ok"),message='Variable must have both dimensions greater than 2!',caption="Input Error")
			}
		}
	}
}
DST_mahalanobis_MCD<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			if((nrow(M)!=1)&(ncol(M)!=1)){
				v.mcd<-covMcd(M)
				var.dist<-as.matrix(sqrt(mahalanobis(M,v.mcd$center,cov=v.mcd$cov)))
				assign('var.dist',var.dist,envir=.GlobalEnv)
				print('The value is saved in: var.dist')
				print(var.dist)
			}else{
				tk_messageBox(type=c("ok"),message='Variable must have both dimensions greater than 2!',caption="Input Error")
			}
		}
	}
}
DST_manhattan<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			if((nrow(M)!=1)&(ncol(M)!=1)){
				var.dist<-as.matrix(dist(M,method="manhattan"))
				assign('var.dist',var.dist,envir=.GlobalEnv)
				print('The value is saved in: var.dist')
				print(var.dist)
			}else{
				tk_messageBox(type=c("ok"),message='Variable must have both dimensions greater than 2!',caption="Input Error")
			}
		}
	}
}
DST_maximum<-function(previous.name=''){
	ans<-inpboxe1('*Matrix',previous.name)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		M<-as.matrix(M)
		if(sum(is.na(M))!=0){
			print('>>NA found: remove them before evaluation<<')
		}else{
			if((nrow(M)!=1)&(ncol(M)!=1)){
				var.dist<-as.matrix(dist(M,method="maximum"))
				assign('var.dist',var.dist,envir=.GlobalEnv)
				print('The value is saved in: var.dist')
				print(var.dist)
			}else{
				tk_messageBox(type=c("ok"),message='Variable must have both dimensions greater than 2!',caption="Input Error")
			}
		}
	}
}
PCA_biplot<-function(){
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxe3k2(c('*Component on x-axis','*Component on y-axis','Label Vector','Column Names','Arrows'),
		c('1','2','None','TRUE','TRUE'))
		if(!is.null(ans)){
			tex<-as.character(1:PCA[[1]]@nObs)
			if(ans[[3]]!='None')tex<-givemat(ans[[3]],nl=PCA[[1]]@nObs)
			# draw score points
			c1<-as.numeric(ans[[1]])
			c2<-as.numeric(ans[[2]])
			S<-PCA[[1]]@scores
			V<-PCA[[1]]@R2
			Slim<-c(min(S[,c(c1,c2)]),max(S[,c(c1,c2)]))
			Slim<-c(sign(Slim[1])*max(abs(Slim)),sign(Slim[2])*max(abs(Slim)))
			xl<-paste('Comp.',as.character(c1),' (var.',as.character(format(V[c1]*100,digit=2)),'%)',sep='')
			yl<-paste('Comp.',as.character(c2),' (var.',as.character(format(V[c2]*100,digit=2)),'%)',sep='')
			tl=paste('Biplot (var. ',as.character(format((V[c1]+V[c2])*100,digit=2)),'%)',sep='')
			op<-par(pty='s')
			if(is.null(tex)){
				plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,pty='o',xlab=xl,ylab=yl,col='black')
			}else{
				plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,xlab=xl,ylab=yl,type='n')
				text(S[,c(c1,c2)],as.character(tex),col='black',cex=0.8)
			}
			par(op)
			# draw loading arrows
			par(new=TRUE)
			T<-PCA[[1]]@loadings
			tex<-1:nrow(T)
			if(as.logical(ans[[4]]))tex<-rownames(T)
			Tlim<-c(min(T[,c(c1,c2)]),max(T[,c(c1,c2)]))
			Tlim<-c(sign(Tlim[1])*max(abs(Tlim)),sign(Tlim[2])*max(abs(Tlim)))
			plot(T[,c(c1,c2)],axes=FALSE,type='n',xlim=Tlim,ylim=Tlim,pty='s',xlab=xl,ylab=yl)
			if(as.logical(ans[[5]]))arrows(rep(0,dim(T)[1]),rep(0,dim(T)[2]),T[,c1],T[,c2],col='red')
			text(T[,c1],T[,c2],as.character(tex),cex=0.8,col='red')
			axis(side=4)
			axis(side=3)
			par(new=FALSE)
			# draw center and grid
			grid()
			text(0,0,'+',cex=1.2,col='red')
			title(main=tl,line=2.5)
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_cumulative_var_plot<-function(){
	if (exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		op<-par(pty='s')
		V<-PCA[[1]]@R2cum*100
		plot(V,xlab='Number of Components',ylab="% Explained Variance",main='Cumulative Variance Plot',ylim=c(0,100),type='n')
		for(i in 1:length(V)){
			if(V[i]!=0)points(i,V[i],col='red')
		}
		lines(1:i,V[1:i])
		grid()
		par(op)
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_data_reconstruction<-function(previous.name=''){
	ans<-inpboxe4k2(c('*Matrix Name','*Rows to be selected (e.g., 1:10,15)','*Columns to be selected (e.g., 1:3,7)',
	"*Max. number of Components for reconstruction" ,'Centered','Scaled'),c(previous.name,'all','all','10','TRUE','TRUE'))
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
		if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
		if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
		if((typeof(M)=='double')|(typeof(M)=='list')){
			assign('previous.name',ans[[1]],envir=.GlobalEnv)
			M.na<-is.na(M)
			if(sum(is.na(M))!=0){
				sc<-"none"
				if(as.logical(ans[[6]]))sc<-"uv"
				pre<-as.logical(ans[[5]])
				npc<-min(as.numeric(ans[[4]]),dim(M))
				res<-pca(M,method="nipals",center=pre,scale=sc,nPcs=npc)
				V_<-res@R2*100
				plot(V_,xlab='Component Number',ylab="% Explained Variance",main='% Explained Variance',ylim=c(0,max(V_)*1.2),type='n')
				for(i in 1:length(V_)){
					if(V_[i]!=0)points(i,V_[i],col='blue')
				}
				lines(1:i,V_[1:i]);grid()
				ans<-inpboxe1('*Number of Components for reconstruction',as.character(npc))
				if(!is.null(ans))npc<-as.numeric(ans[[1]])
				M.rec<-fitted(res,nPcs=npc,pre=pre,post=TRUE)
				M.rec[!M.na]<-M[!M.na]
				M.rec<-as.data.frame(M.rec)
				names(M.rec)<-names(M)
				row.names(M.rec)<-row.names(M)
				assign('M.rec',M.rec,envir=.GlobalEnv)
				eval(parse(text=paste(previous.name,'.old','<-',previous.name,sep='')),envir=.GlobalEnv)
				eval(parse(text=paste(previous.name,'<-M.rec',sep='')),envir=.GlobalEnv)
				print(paste('Original matrix is saved in: ',previous.name,'.old',sep=''))
			}else{
				tk_messageBox(type=c("ok"),message='No Missing Data!',caption='Input Error')
			}
		}
	}
}
PCA_diagnostic_cont_plot<-function(){
	# internal function definition
	pcaconplot<-function(i,PCA,n,m,ncp,lbl,nm){
		X<-as.matrix(PCA[[1]]@completeObs)
		P<-PCA[[1]]@loadings[,1:ncp]
		S<-PCA[[1]]@scores[,1:ncp]
		Ls<-PCA[[1]]@sDev[1:ncp]#not variance because the sum must be the Malanobis distance
		sgl<-sum(Ls)
		sgr<-PCA$sgt-sgl
		MQ<-S%*%t(P)
		MT<-P%*%diag(1/Ls,ncp,ncp)%*%t(P)
		T<-X%*%MT
		Q<-sign(X-MQ)*(X-MQ)^2
		Ti<-T[i,]
		Qi<-Q[i,]
		Qlim<-c(min(0,Qi),max(0,Qi))
		Tlim<-c(min(0,Ti),max(0,Ti))
		if(nm){
			Ti<-Ti/apply(abs(T),2,quantile,probs=0.95)
			Qi<-Qi/apply(abs(Q),2,quantile,probs=0.95)
			Qlim<-c(min(Qi,-1.1),max(Qi,1.1))
			Tlim<-c(min(Ti,-1.1),max(Ti,1.1))
		}
		op<-par(mfrow=c(1,2))
		options(scipen=1)
		barplot2(Qi,main=paste('Qi of object:',i),ylim=Qlim,cex.lab=1.2,names.arg=lbl,cex.names=0.6,plot.grid=TRUE,las=2,cex.axis=0.6)
		box(which="plot",lty="solid")
		if(nm)abline(h=1,col='red')
		if(nm)abline(h=-1,col='red')
		barplot2(Ti,main=paste('Ti^2 of object:',i),ylim=Tlim,cex.lab=1.2,names.arg=lbl,cex.names=0.6,plot.grid=TRUE,las=2,cex.axis=0.6)
		box(which="plot",lty="solid")
		if(nm)abline(h=1,col='red')
		if(nm)abline(h=-1,col='red')
		par(op)
		return()
	}
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxc2k(c('*Row number','*Number of Components','Normalized'),as.character(1:PCA[[1]]@nObs),as.character(1:PCA[[1]]@nPcs),c('0','1','FALSE'))
		if(!is.null(ans)){
			vc<-as.numeric(ans[[1]])
			ncp<-as.numeric(ans[[2]])
			nm<-as.logical(ans[[3]])
			nc<-PCA[[1]]@nVar
			nr<-PCA[[1]]@nObs
			lbl<-names(as.data.frame(PCA[[2]]))
			if(ncp<=nc){
				pcaconplot(vc,PCA,nr,nc,ncp,lbl,nm)
			}else{
				tk_messageBox(type = c("ok"),message='Number of component greater than number of variables!',caption="Input Error")
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_cont_plot_Dataset<-function(){
	# internal function definition
	pcaconplot_testset<-function(X,i,PCA,ncp,lbl,nm){
		nr<-nrow(X)
		nc<-ncol(X)
		unity<-matrix(rep(1,nr),nr,1)
		if(PCA$center)X<-X-(unity%*%PCA$centered)
		if(PCA$scale)X<-X/(unity%*%PCA$scaled)
		X<-as.matrix(X)
		P<-PCA[[1]]@loadings[,1:ncp]
		S<-X%*%P
		Ls<-PCA[[1]]@sDev[1:ncp]
		sgl<-sum(Ls)
		sgr<-PCA$sgt-sgl
		MQ<-S%*%t(P)
		MT<-P%*%diag(1/Ls,ncp,ncp)%*%t(P)
		T<-X%*%MT
		Q<-sign(X-MQ)*(X-MQ)^2
		Ti<-T[i,]
		Qi<-Q[i,]
		Qlim<-c(min(0,Q),max(0,Q))
		Tlim<-c(min(0,T),max(0,T))
		if(nm){
			Ti<-Ti/apply(abs(T),2,quantile,probs=0.95)
			Qi<-Qi/apply(abs(Q),2,quantile,probs=0.95)
			Qlim<-c(min(Qi,-1.1),max(Qi,1.1))
			Tlim<-c(min(Ti,-1.1),max(Ti,1.1))
		}
		op<-par(mfrow=c(1,2))
		options(scipen=1)
		barplot2(Qi,main='Q',ylim=Qlim,
		cex.lab=1.2,names.arg=lbl,cex.names=0.6,plot.grid=TRUE,las=2,cex.axis=0.6)
		box(which="plot",lty="solid")
		if(nm)abline(h=1,col='red')
		if(nm)abline(h=-1,col='red')
		barplot2(Ti,main='T^2',ylim=Tlim,
		cex.lab=1.2,names.arg=lbl,cex.names=0.6,plot.grid=TRUE,las=2,cex.axis=0.6)
		box(which="plot",lty="solid")
		if(nm)abline(h=1,col='red')
		if(nm)abline(h=-1,col='red')
		par(op)
		return()
	}
	if(exists("PCA",envir=.GlobalEnv)){
		ans<-inpboxe4ck(c('*External Data Set','*Row number (e.g.,10)','*Columns to be selected (e.g., 1:3,7)',
		'External Vector with Variable Names (e.g., A[1,])','*Number of Components','Normalized'),as.character(1:PCA[[1]]@nPcs),
		c(previous.name,'all','all','None',1,'FALSE'))
		if(!is.null(ans)){
			M<-eval(parse(text=ans[[1]]))
			if(sum(is.na(M))!=0){
				print('>>NA found: remove them before evaluation<<')
			}else{
				if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
				if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
				if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
				lbl<-names(as.data.frame(PCA[[2]]))
				if(ans[[4]]!='None')lbd<-eval(parse(text=ans[[4]]))
				ncp<-as.numeric(ans[[5]])
				nm<-as.logical(ans[[6]])
				nc<-PCA[[1]]@nVar
				if(ncp<=nc){
					pcaconplot_testset(M,1,PCA,ncp,lbl,nm)
				}else{
					tk_messageBox(type = c("ok"),message='Number of component greater than number of variables!',caption="Input Error")
				}
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_diagnostic_plot_t2q<-function(){
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		if(PCA$type=='pca'){
			ans<-inpboxc('Number of Components',as.character(1:PCA[[1]]@nPcs),-1)
			if(!is.null(ans)){
				ncp<-ans[[1]]
				n<-PCA[[1]]@nObs
				m<-PCA[[1]]@nVar
				X<-as.matrix(PCA[[1]]@completeObs)
				P<-as.matrix(PCA[[1]]@loadings[,1:ncp])
				L<-as.vector((PCA[[1]]@sDev[1:ncp])^2)
				MQ<-diag(rep(1,m))-(P%*%t(P))
				MT<-P%*% (diag(length(L))*(1/L))%*%t(P)
				Q<-diag(X%*%MQ%*%t(X))
				T<-diag(X%*%MT%*%t(X))
				Qlim<-10^(mean(log10(Q))+qt(0.95,n-1)*sd(log10(Q)))
				Tlim<-(n-1)*(n+1)*ncp/n/(n-ncp)*qf(0.95,ncp,n-ncp)
				if(is.na(Tlim))Tlim<-0
				mT<-max(T,Tlim)
				mQ<-max(Q,Qlim)
				op<-par(mfrow=c(1,2))
				plot(Q,ylim=c(0,1.1*mQ),ylab="Q Index",xlab="Sample number",cex.lab=1.2)
				abline(h=Qlim,lty=2,col='red')
				xtx<-(1:n)[Q>Qlim];ytx<-Q[Q>Qlim];tx<-as.character(xtx)
				if(length(xtx)!=0)text(xtx,ytx,label=tx,cex=0.5,pos=3)
				title(main=paste("Line: crit. val. at p=0.05, Number of components: ",ncp),cex.main=0.6)
				plot(T,ylim=c(0,mT*1.1),ylab="T^2 Hotelling Index",xlab="Sample number",cex.lab=1.2)
				abline(h=Tlim,lty=2,col='red')
				xtx<-(1:n)[T>Tlim];ytx<-T[T>Tlim];tx<-as.character(xtx)
				if(length(xtx)!=0)text(xtx,ytx,label=tx,cex=0.5,pos=3)
				title(main=paste("Line: crit. val. at p=0.05, Number of components:",ncp),cex.main=0.6);par(op)
			}
		}else{
			tk_messageBox(type=c("ok"),message='Function not allowed with Varimax!',caption="Input Error")
		}			
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_diagnostic_plot_t2vsq<-function(){
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		if(PCA$type=='pca'){
			ans<-inpboxc('Number of Components',as.character(1:PCA[[1]]@nPcs),-1)
			if(!is.null(ans)){
				ncp<-as.numeric(ans[[1]])
				n<-PCA[[1]]@nObs
				m<-PCA[[1]]@nVar
				X<-as.matrix(PCA[[1]]@completeObs)
				P<-as.matrix(PCA[[1]]@loadings[,1:ncp])
				L<-as.vector((PCA[[1]]@sDev[1:ncp])^2)
				MQ<-diag(rep(1,m))-(P%*%t(P))
				MT<-P%*%(diag(length(L))*(1/L))%*%t(P)
				Q<-diag(X%*%MQ%*%t(X))
				T<-diag(X%*%MT%*%t(X))
				Qlim<-10^(mean(log10(Q))+qt(0.95,n-1)*sd(log10(Q)))
				Tlim<-(n-1)*(n+1)*ncp/n/(n-ncp)*qf(0.95,ncp,n-ncp)
				if(is.na(Tlim))Tlim<-0
				mT<-max(T,Tlim)
				if(is.na(Qlim))Qlim<-0
				mQ<-max(Q,Qlim)
				plot(T,Q,ylim=c(0,mQ*1.1),xlim=c(0,mT*1.1),ylab="Q Index",xlab="T^2 Hotelling Index",cex.lab=1.2)
				title(main=paste("Number of components:",ncp),sub='Dashed lines: critical values at p=0.05',cex.sub=0.6)
				grid()
				if(Tlim!=0)abline(v=Tlim,lty=2,col='red')
				if(Qlim!=0)abline(h=Qlim,lty=2,col='red')
				if((Tlim!=0)|(Qlim!=0)){
					QT<-data.frame(Q=Q,T=T,tx=1:n)
					QTs<-subset(QT,((T>Tlim)|(Q>Qlim)))
					if(nrow(QTs)!=0)text(QTs$T,QTs$Q,label=QTs$tx,cex=0.5,pos=3)
                PCA$h2q<-list(MQ=MQ,MT=MT,Q=Q,T=T,Qlim=Qlim,Tlim=Tlim)
                assign('PCA',PCA,envir=.GlobalEnv)
				}
			}
		}else{
			tk_messageBox(type=c("ok"),message='Function not allowed with Varimax!',caption="Input Error")
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_explained_variance_variable<-function(){
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxc('Number of Components:',as.character(1:PCA$res@nPcs),-1)
		if(!is.null(ans)){
			if(PCA[[1]]@scaled=='uv')scale<-TRUE else scale<-FALSE
			pcaVarexpl(PCA$dataset,a=ans[[1]],scale=scale,center=PCA[[1]]@centered,las=2,cex.names=0.8,
			main='Variance expl. by each Variable');box(lty=1,col='red') 
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_extract<-function(){
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxc('Extract Matrix:',c('Norm.Var.','Loadings','Scores','Cum.Var','sDev','Variance','Eigenvalues','Identity'))
		if(!is.null(ans)){
			if(ans[[1]]==1){
				nVm<-PCA[[1]]@R2
				assign('nVm',nVm,envir=.GlobalEnv)
				print('Values saved in nVm vector',quote=FALSE)
			}
			if(ans[[1]]==2){
				Lm<-t(PCA[[1]]@loadings)
				assign('Lm',Lm,envir=.GlobalEnv)				
				print('Values saved in Lm matrix',quote=FALSE)
			}
			if(ans[[1]]==3){
				Sm<-PCA[[1]]@scores
				assign('Sm',Sm,envir=.GlobalEnv)
				print('Values saved in Sm matrix',quote=FALSE)
			}
			if(ans[[1]]==4){
				CVm<-PCA[[1]]@R2cum
				assign('CVm',CVm,envir=.GlobalEnv)
				print('Values saved in CVm vector',quote=FALSE)
			}
			if(ans[[1]]==5){
				sDm<-PCA[[1]]@sDev
				assign('sDm',sDm,envir=.GlobalEnv)
				print('Values saved in sDm vector',quote=FALSE)
			}
			if(ans[[1]]==6){
				Vm<-PCA[[1]]@sDev^2
				assign('Vm',Vm,envir=.GlobalEnv)
				print('Values saved in Vm vector',quote=FALSE)
			}
			if(ans[[1]]==7){
				Sm<-PCA[[1]]@scores
				Sm<-as.matrix(Sm)
				Eig<-t(Sm)%*%(Sm)
				assign('Eig',Eig,envir=.GlobalEnv)
				print('Values saved in Eig matrix',quote=FALSE)
				print(paste('Lamda0 ',format(matrix.trace(Eig),digits=6),sep=''),quote=FALSE)
			}
			if(ans[[1]]==8){
				Lm<-t(PCA[[1]]@loadings)
				Lm<-as.matrix(Lm)
				Ide<-t(Lm)%*%(Lm)
				assign('Ide',Ide,envir=.GlobalEnv)
				print('Values saved in Ide matrix',quote=FALSE)
				print(paste('sum.diag ',format(matrix.trace(Ide),digits=6),sep=''),quote=FALSE)
			}

		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCA!',caption="Input Error")
	}
}
PCA_loading_plot_bar<-function(){
	# internal function definition
	plotco<-function(T,c1=1,label=NULL){
		nr<-nrow(T)
		if(is.null(label))label<-as.character(1:nr)
		# fix the printing area 5x5 inches
		par(fin=c(5,5))
		barplot(T[,c1],main=paste('Loading on Comp ',as.character(c1),sep='.'),names.arg=as.character(label),cex.names=0.8,las=2)
		box(lty=1,col='red')
		grid()
		return()
	}
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxcke(c('Component Number','Column Names','Label Vector'),1:PCA$res@nPcs,c('-1','FALSE',''))
		if(!is.null(ans)){
			lb<-1:PCA$res@nVar
			if(as.logical(ans[[2]]))lb<-names(as.data.frame(PCA[[2]]))
			if(ans[[3]]!='')lb<-givemat(ans[[3]],nl=PCA$res@nVar)
			plotco(PCA[[1]]@loadings,as.numeric(ans[[1]]),lb)
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_loading_plot_scatter<-function(){
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxe3k2(c('* Component on x-axis','* Component on y-axis','Label Vector (e.g., A[1,])','Column Names','Arrows'),
		c('1','2','None','FALSE','TRUE'))
		if(!is.null(ans)){
			c1<-as.numeric(ans[[1]])
			c2<-as.numeric(ans[[2]])
			tex<-as.character(1:ncol(PCA$dataset))
			if(ans[[3]]!='None'){
				tex<-givemat(ans[[3]],nl=ncol(PCA$dataset))
			}
			if(as.logical(ans[[4]]))tex<-names(PCA$dataset)
			T<-PCA[[1]]@loadings
			V<-PCA[[1]]@R2
			Tlim<-c(min(T[,c(c1,c2)]),max(T[,c(c1,c2)]))
			Tlim<-c(sign(Tlim[1])*max(abs(Tlim)),sign(Tlim[2])*max(abs(Tlim)))
			xl<-paste('Comp.',as.character(c1),' (var.',as.character(format(V[c1]*100,digit=2)),'%)',sep='')
			yl<-paste('Comp.',as.character(c2),' (var.',as.character(format(V[c2]*100,digit=2)),'%)',sep='')
			tl<-paste('Loading Plot (var. ',as.character(format((V[c1]+V[c2])*100,digit=2)),'%)',sep='') 
			op<-par(pty='s')
			if(is.null(tex))tex<-as.character(1:nrow(T))
			plot(T[,c(c1,c2)],type='n',xlim=Tlim,ylim=Tlim,pty='s',xlab=xl,ylab=yl,main=tl)
			if(as.logical(ans[[5]])){
				arrows(rep(0,dim(T)[1]),rep(0,dim(T)[2]),T[,c1],T[,c2],col='red')
				grid()
			}
			text(T[,c1],T[,c2],as.character(tex),cex=0.8)
			text(0,0,'+',cex=1.2,col='red')
			par(op)
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_model_PCA<-function(previous.name=''){
    if(exists("PCA",envir=.GlobalEnv))rm("PCA",envir=.GlobalEnv)
	PCA<-list()
	if(!exists('pca.set'))pca.set<-c(previous.name,'all','all','5','TRUE','TRUE')
	ans<-inpboxe4k2(c('* Matrix Name','* Rows to be selected (e.g., 1:10,15)','* Columns to be selected (e.g., 1:3,7)',
	'* Number of Components','Centered','Scaled'),pca.set)
	if(!is.null(ans)){
		name<-ans[[1]]
		M<-givemat(name)
		if(!is.null(M)){
			assign('previous.name',name,envir=.GlobalEnv)
			pca.set<-ans
			if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
			if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
			if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
			if((typeof(M)=='double')|(typeof(M)=='list')){
				nNA<-sum(is.na(M))
				if(nNA>0){
					mess<-paste(as.character(nNA),'NA present.We try to rebuild them!')
					tk_messageBox(type=c("ok"),message=mess,caption="Input Error")
				}
				ncom<-as.numeric(ans[[4]])
				if((ncom>ncol(M))|(ncom<1)){
					tk_messageBox(type=c("ok"),message='Wrong component number !',caption="Input Error")
				}else{
					sgt<-as.integer(ans[[4]])
					if(!as.logical(ans[[6]]))sgt<-sum(apply(M,2,var))
					ccs<-'none';if(as.logical(ans[[6]]))ccs<-'uv'
					md<-prep(M,scale=ccs,center=as.logical(ans[[5]]),simple=FALSE,reverse=FALSE)
					res<-pca(md$data,method="nipals",nPcs=as.numeric(ans[[4]]),scale=ccs,center=as.logical(ans[[5]]))
					PCA$res<-res
					PCA$dataset<-prep(PCA$res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
					PCA$dataset<-as.data.frame(PCA$dataset)
					PCA$center<-ans[[5]]
					PCA$scale<-ans[[6]]
					PCA$centered<-md$center
					PCA$scaled<-md$scale
					PCA$sgt<-sgt
					PCA$type<-'pca'
					assign('PCA',PCA,envir=.GlobalEnv)
					print('Note : Data are saved in the PCA object, write PCA to see all',quote=FALSE)
					print(paste('Variance Explained by the',res@nPcs,'components :',
					format(res@R2cum[res@nPcs]*100,digit=3),'%'),quote=FALSE)
					print('% Variance explained by each component:',quote=FALSE)
					print(format(res@R2*100,digit=3),quote=FALSE)
				}
			}
		}
	}
}
PCA_model_varimax<-function(previous.name=''){
	PCA_model_PCA(previous.name='')
	PCA_variance_plot()
	ans<-inpboxc('*Number of components for Varimax rotation',2:PCA$res@nPcs,-1)
	if(!is.null(ans)){
		ncomp<-ans[[1]]+1
		prl<-t(PCA$res@loadings[,1:ncomp])
		go<-1
		while(go==1){
			for(i in 1:(ncomp-1)){
				for(j in (i+1):ncomp){
					lo<-prl[c(i,j),]
					rotb<-0
					sim<-sum(lo^4)
					simmax<-sim
					for (rot in seq(-90,90,0.1)){
						RM<-c(cos(rot*pi/180),-sin(rot*pi/180),sin(rot*pi/180),cos(rot*pi/180))
						RM<-matrix(RM,2,2)
						lo2<-RM%*%lo
						sim2<-sum(lo2^4)
						if(sim2>simmax){
							lob<-lo2
							simmax<-sim2
							rotb<-rot
						}
					}
					if(rotb!=0){
						go<-1
						prl[i,]<-lob[1,]
						prl[j,]<-lob[2,]
					}else{
						go<-0
					}
				}
			}
		}
		prs<-PCA$res@completeObs%*%t(prl)
		vp<-apply(prs^2,2,sum)/sum(apply(PCA$res@completeObs^2,2,sum))
		ivp<-sort(vp,decreasing=TRUE,index.return=TRUE)$ix
		vp<-sort(vp,decreasing=TRUE,index.return=TRUE)$x
		PCA$res@loadings<-PCA$res@loadings[,1:ncomp]
		PCA$res@scores<-PCA$res@scores[,1:ncomp]
		name.pca<-colnames(PCA$res@loadings)
		PCA$res@loadings<-t(prl[ivp,])
		PCA$res@scores<-prs[,ivp]
		names(PCA$res@loadings)<-name.pca
		names(PCA$res@scores)<-name.pca
		names(vp)<-name.pca
		PCA$res@nPcs<-ncomp
		PCA$res@R2<-vp
		PCA$res@sDev<-sqrt(PCA$res@R2)
		PCA$res@R2cum<-cumsum(PCA$res@R2)
		PCA$type<-'varimax'
		print('*****',quote=FALSE)
		print(paste('VARIMAX: Variance Explained by the',ncomp,'components :',
		format(PCA$res@R2cum[PCA$res@nPcs]*100,digit=3),'%'),quote=FALSE)
		print('% for components:',quote=FALSE)
		print(format(PCA$res@R2*100,digit=3),quote=FALSE)
	}
}
PCA_number_pcs_determination<-function(previous.name=''){
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxe3k2(c('Dataset Name ','Segment     ','Replicates  ','Centered','Scaled'),c(previous.name,'4','50','TRUE','TRUE'))
		if(!is.null(ans)){
			name<-ans[[1]]
			M<-givemat(name)
			M<-scale(M,center=as.logical(ans[[4]]),scale=as.logical(ans[[5]]))
			pcaCV(M,amax=min(10,ncol(M)),segments=as.numeric(ans[[2]]),repl=as.numeric(ans[[3]]))
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_projection_training_set<-function(previous.name=''){
	# internal function definition
	dellipse<-function (x,me=c(0,0),covm=cov(x),q=0.95) 
	{   m<-100
		cov.svd<-svd(covm,nv=0)
		r<-cov.svd[["u"]]%*%diag(sqrt(cov.svd[["d"]]))
		alphamd<-sqrt(qchisq(q,2))
		e1md<-cos(c(0:m)/m*2*pi)*alphamd
		e2md<-sin(c(0:m)/m*2*pi)*alphamd
		emd<-cbind(e1md,e2md)
		ttmd<-t(r%*%t(emd))+rep(1,m+1)%o% me
		xmax<-max(c(x[,1],ttmd[,1]))
		xmin<-min(c(x[,1],ttmd[,1]))
		ymax<-max(c(x[,2],ttmd[,2]))
		ymin<-min(c(x[,2],ttmd[,2]))
		sdx<-sd(x[,1])
		sdy<-sd(x[,2])
		e1md<-cos(c(0:m)/m*2*pi)*alphamd
		e2md<-sin(c(0:m)/m*2*pi)*alphamd
		emd<-cbind(e1md,e2md)
		ttmd<-t(r%*%t(emd))+rep(1,m+1)%o%me
		return(ttmd)
	}
	# function body
	if (exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxe7k2(c('*External Data Set','*Rows to be selected (e.g., 1:10,15)','*Columns to be selected (e.g., 1:3,7)',
		'Label Vector for external set (e.g., A[,1])','*Component on x-axis','*Component on y-axis','Label Vector for training set (e.g., A[,1])',
		'Row Names','Ellipse'),c('','all','all','None','1','2','None','FALSE','FALSE'))
		if(!is.null(ans)){
			if((as.logical(ans[[9]]))&(!PCA$center | !PCA$scale)){
				ans<-NULL
				tk_messageBox(type=c("ok"),message='No Ellipse without Autoscale!',caption="Input Error")
			}
			c1<-as.integer(ans[[5]])
			c2<-as.integer(ans[[6]])
			lb_<-NULL
			if(as.logical(ans[[8]]))lb_<-row.names(PCA$dataset)
			if(ans[[7]]!='None')lb_<-givemat(ans[[7]],nl=nrow(PCA$dataset))
			M<-givemat(ans[[1]])
			if(sum(is.na(M))!=0){
				print('>>NA found: remove them before evaluation<<')
			}else{
				if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
				if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
				if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
				lbd<-NULL
				if(as.logical(ans[[8]]))lbd<-row.names(M)
				if(ans[[4]]!='None')lbd<-givemat(ans[[4]],nl=nrow(M))
				# standard score evaluation
				S<-PCA$res@scores
				ME_<-matrix(0,1,2)
				if(as.logical(ans[[9]]))ME_<-dellipse(S[,c(c1,c2)])
				v1_<-PCA$res@R2[c1]*100
				v2_<-PCA$res@R2[c2]*100
				yn.lb<-TRUE
				if(is.null(lb_))yn.lb<-FALSE
				if(yn.lb){
					if(length(lb_)!=nrow(S)){
						tk_messageBox(type=c("ok"),message='Wrong Score Label Dimension !',caption="Input Error")
					}
				}
				# new dataset evaluation
				T_<-PCA$res@loadings
				unity<-matrix(rep(1,nrow(M)),nrow(M),1)
				if(PCA$center)M<-M-(unity%*%PCA$centered)
				if(PCA$scale)M<-M/(unity%*%PCA$scaled)
				D<-as.matrix(M) %*% T_
				# plot standard score plot in the new scale 
				Slim<-c(min(S[,c(c1,c2)],D[,c(c1,c2)],ME_),max(S[,c(c1,c2)],D[,c(c1,c2)],ME_))
				xl_<-paste('Comp.',as.character(c1),' (var.',as.character(format(v1_,digit=2)),'%)',sep='')
				yl_<-paste('Comp.',as.character(c2),' (var.',as.character(format(v2_,digit=2)),'%)',sep='')
				tl_=paste('Score Plot (var. ',as.character(format((v1_+v2_),digit=2)),'%)',sep='')
				op<-par(pty='s')
				if(!yn.lb){
					plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,pty='o',xlab=xl_,ylab=yl_,col='black')
				}
				if(yn.lb){
					plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,xlab=xl_,ylab=yl_,type='n')
					text(S[,c(c1,c2)],as.character(lb_),col='black')}
					grid()
				if(as.logical(ans[[9]])){
					lines(ME_,col='red')
					title(main=tl_,sub='Training: black - External: red - Ellipse:95%',cex.main=1.5,font.main=2,
					col.main="black",cex.sub=0.75,font.sub=2,col.sub="red")
				}else{
					title(main=tl_,sub='Training: black - External: red',cex.main=1.5,font.main=2,
					col.main="black",cex.sub=0.75,font.sub=2,col.sub="red")
				}
				# new dataset plot
				ynld<-TRUE
				nd<-nrow(D)
				if(is.null(lbd))ynld<-FALSE 
				if(ynld){
					if(length(lbd)!=nd){
						tk_messageBox(type=c("ok"),message='Wrong Dataset Label Dimension !',caption="Input Error")
					}
				}
				if(!ynld)points(D[,c1],D[,c2],col='red')
				if(ynld){points(D[,c1],D[,c2],type='n')
				text(D[,c1],D[,c2],as.character(lbd),col='red')}
				par(op)
				# save new coordinates
				assign('new.coo',D,envir=.GlobalEnv)
				print('New Coordinates are saved in matrix: new.coo')
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_score_plot<-function(){
	# internal function definition
	dellipse<-function (x,me=c(0,0),covm=cov(x),q=0.95) 
	{   m<-100
		cov.svd<-svd(covm,nv=0)
		r<-cov.svd[["u"]]%*%diag(sqrt(cov.svd[["d"]]))
		alphamd<-sqrt(qchisq(q,2))
		e1md<-cos(c(0:m)/m*2*pi)*alphamd
		e2md<-sin(c(0:m)/m*2*pi)*alphamd
		emd<-cbind(e1md,e2md)
		ttmd<-t(r%*%t(emd))+rep(1,m+1)%o% me
		xmax<-max(c(x[,1],ttmd[,1]))
		xmin<-min(c(x[,1],ttmd[,1]))
		ymax<-max(c(x[,2],ttmd[,2]))
		ymin<-min(c(x[,2],ttmd[,2]))
		sdx<-sd(x[,1])
		sdy<-sd(x[,2])
		e1md<-cos(c(0:m)/m*2*pi)*alphamd
		e2md<-sin(c(0:m)/m*2*pi)*alphamd
		emd<-cbind(e1md,e2md)
		ttmd<-t(r%*%t(emd))+rep(1,m+1)%o%me
		return(ttmd)
	}
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		ans<-inpboxe4k2(c('*Component on x-axis','*Component on y-axis','Label Vector (e.g., A[,1])','Color Vector (e.g., A[,1])','Row Names','Ellipse'),
		c('1','2','None','None','FALSE','FALSE'))
		if(!is.null(ans)){
			if((as.logical(ans[[6]]))&(!PCA$center | !PCA$scale)){
				ans<-NULL
				tk_messageBox(type=c("ok"),message='No Ellipse without Autoscale!',caption="Input Error")
			}
			c1<-as.numeric(ans[[1]])
			c2<-as.numeric(ans[[2]])
			tex<-NULL
			grade<-NULL
			if(as.character(ans[[3]])!='None')tex<-givemat(ans[[3]],nl=nrow(PCA$dataset))
			if(as.logical(ans[[5]]))tex<-rownames(PCA$dataset)
			if(as.character(ans[[4]])!='None'){
				grade<-givemat(ans[[4]],nl=nrow(PCA$dataset))
			}
			if(!is.null(grade)){
				tog<-typeof(grade)
				if(is.factor(grade))tog<-"factor"
				print(tog)
				grade<-factor(grade)
				lev<-levels(grade)
				nl<-nlevels(grade)
				if(tog=="double")vcolor<-unlist(dovc(as.numeric(lev)))
				if(tog=="factor")vcolor<-unlist(dovc(as.character(lev)))
				if(tog=="character")vcolor<-unlist(dovc(as.character(lev)))
				if(tog=="integer")vcolor<-unlist(dovc(as.numeric(lev)))
			}
			S<-PCA[[1]]@scores
			V<-PCA[[1]]@R2
			ME<-matrix(0,1,2)
			if(as.logical(ans[[6]]))ME<-dellipse(S[,c(c1,c2)])
			Slim<-c(min(S[,c(c1,c2)],ME),max(S[,c(c1,c2)],ME))
			xl<-paste('Comp.',as.character(c1),' (var.',as.character(format(V[c1]*100,digit=2)),'%)',sep='')
			yl<-paste('Comp.',as.character(c2),' (var.',as.character(format(V[c2]*100,digit=2)),'%)',sep='')
			tl<-paste('Score Plot (var. ',as.character(format((V[c1]+V[c2])*100,digit=2)),'%)',sep='')
			op<-par(pty='s')
			if(is.null(tex) & is.null(grade)){
				plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,pty='o',xlab=xl,ylab=yl,main=tl,col='black')
				grid()
			}
			if(!is.null(tex)& is.null(grade)){
				plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,xlab=xl,ylab=yl,main=tl,type='n')
				grid()
				text(S[,c(c1,c2)],as.character(tex),col='black',cex=0.8)
			}
			if(is.null(tex)& !is.null(grade)){
				plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,xlab=xl,ylab=yl,main=tl,type='n')
				grid()
				for(i in 1:nl){
					points(subset(S[,c(c1,c2)],grade==lev[i]),pch=19,col=vcolor[i])
					end
				}
			}
			if(!is.null(tex)& !is.null(grade)){
				plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,xlab=xl,ylab=yl,main=tl,type='n')
				grid()
				for(i in 1:nl){
					text(subset(S[,c(c1,c2)],grade==lev[i]),as.character(subset(tex,grade==lev[i])),col=vcolor[i],cex=0.8)
					end
				}
			}
			text(0,0,'+',cex=1.2,col='red')
			if(as.logical(ans[[6]])){
				lines(ME,col='red')
				title(main=NULL,sub='Ellipse: critical T^2 value at p=0.05',col.sub='red',cex.sub=0.6)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_t2vsq_Dataset<-function(previous.name=''){
	# internal function definition
	pcanewdia<-function(PCA,n,m,ncp,M,lbd){
		X<-as.matrix(PCA[[1]]@completeObs)
		P<-as.matrix(PCA[[1]]@loadings[,1:ncp])
		L<-as.vector((PCA[[1]]@sDev[1:ncp])^2)
		MQ<-diag(rep(1,m))-(P%*%t(P))
		MT<-P%*%(diag(length(L))*(1/L))%*%t(P)
		Q<-diag(X%*%MQ%*%t(X))
		T<-diag(X%*%MT%*%t(X))
		Qlim<-10^(mean(log10(Q))+qt(0.95,n-1)*sd(log10(Q)))
		Tlim<-(n-1)*(n+1)*ncp/n/(n-ncp)*qf(0.95,ncp,n-ncp)
		if(is.na(Tlim))Tlim<-0
		if(is.na(Qlim))Qlim<-0
        PCA$Q<-Q
        PCA$T<-T
        PCA$MQ<-MQ
        PCA$MT<-MT
        PCA$Qlim<-Qlim
        PCA$Tlim<-Tlim
		# new dataset evaluation
		nr<-nrow(M)
		nc<-ncol(M)
		unity<-matrix(rep(1,nr),nr,1)
		if(PCA$center)M<-M-(unity%*%PCA$centered)
		if(PCA$scale)M<-M/(unity%*%PCA$scaled)
		M<-as.matrix(M)
		QN<-diag(M%*%MQ%*%t(M))
		TN<-diag(M%*%MT%*%t(M))
		# plot T^2 vs. Q 
		mQ<-max(Q,QN,Qlim)
		mT<-max(T,TN,Tlim)
		plot(T,Q,ylim=c(0,mQ*1.1),xlim=c(0,mT*1.1),
		ylab="Q Index",xlab="T^2 Hoteling Index",cex.lab=1.2)
		grid()
		tl<-paste("Number of components:",ncp)
		title(main=tl,sub='Training: black - External: red - Dashed lines: critical values at p=0.05',cex.main=1.5,font.main=2,
		col.main="black",cex.sub=0.75,font.sub=2,col.sub="red")
		abline(v=Tlim,lty=2,col='red')
		abline(h=Qlim,lty=2,col='red')
		if(is.null(lbd))points(TN,QN,col='red')
		if(!is.null(lbd))text(TN,QN,as.character(lbd),col='red')
		return(PCA)
	}
	# Menu
	if(exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		if(PCA$type=='pca'){
			ans<-inpboxe5(c('*Number of Components','*External Data Set','*Rows to be selected (e.g., 1:10,15)',
			'*Columns to be selected (e.g., 1:3,7)','Label Vector (e.g., A[,1])'),c('2',previous.name,'all','all','None'))
			if(!is.null(ans)){
				if(as.numeric(ans[[1]])<=PCA[[1]]@nVar){
					M<-givemat(ans[[2]])
					if(sum(is.na(M))!=0){
						print('>>NA found: remove them before evaluation<<')
					}else{
						if((ans[[3]]!='all')&(ans[[4]]!='all'))M<-M[givedim(ans[[3]]),givedim(ans[[4]])]
						if((ans[[3]]!='all')&(ans[[4]]=='all'))M<-M[givedim(ans[[3]]),]
						if((ans[[3]]=='all')&(ans[[4]]!='all'))M<-M[,givedim(ans[[4]])]
						lbd<-NULL
						if(ans[[5]]!='None')lbd<-givemat(ans[[5]])
						PCA<-pcanewdia(PCA,PCA[[1]]@nObs,PCA[[1]]@nVar,as.numeric(ans[[1]]),M,lbd)
                        assign('PCA',PCA,envir=.GlobalEnv)
					}
				}else{
					tk_messageBox(type=c("ok"),message='Number of component greater than number of variables!',caption="Input Error")
				}
			}
		}else{
			tk_messageBox(type=c("ok"),message='Function not allowed with Varimax!',caption="Input Error")
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
PCA_variance_plot<-function(){
	if (exists("PCA",envir=.GlobalEnv)){
		get("PCA",envir=.GlobalEnv)
		op<-par(pty='s')
		V<-PCA[[1]]@R2*100
		plot(V,xlab='Component Number',ylab="% Explained Variance",main='% Explained Variance',ylim=c(0,max(V)*1.2),type='n')
		for(i in 1:length(V)){
			if(V[i]!=0)points(i,V[i],col='blue') 
		}
		lines(1:i,V[1:i])
		grid()
		par(op)
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First!',caption="Input Error")
	}
}
DOE_coefficients<-function(){
	if(exists("DOE",envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		if(DOE$loY){
			x<-DOE$x
			b<-DOE$b
			dof<-DOE$dof
			sig<-DOE$sig
			sdcoeff<-DOE$sdcoeff
			nr<-nrow(x)
			nc<-ncol(x)
			iv<-1
			if(sum(x[,1]==1)==nr)iv<-2
			if(dof==0){
				barplot(b[iv:nc],space=0,col='red',main='Coefficients',names.arg=1:(nc-iv+1))
				box(lty=2)
			}else{
				interv<-qt(0.975,dof)*sdcoeff
				llim<-b-interv
				ulim<-b+interv
				barplot(b[iv:nc],space=0,col='red',main='Coefficients',names.arg=1:(nc-iv+1),
				ylim=c(min(b[iv:nc],llim[iv:nc]),max(b[iv:nc],ulim[iv:nc])))
				box(lty=2)
				s3<-(sig<=0.001)
				for(i in iv:nc){
					if(s3[i])text((i-iv)+0.5,b[i],'***',cex=2)
				}
				s2<-(sig>0.001)&(sig<=0.01)
				for(i in iv:nc){
					if(s2[i])text((i-iv)+0.5,b[i],'**',cex=2)
				}
				s1<-(sig>0.01)&(sig<=0.05)
				for(i in iv:nc){
					if(s1[i])text((i-iv)+0.5,b[i],'*',cex=2)
				}
				for(i in iv:nc){
					segments((i-iv)+0.5,llim[i],(i-iv)+0.5,ulim[i],col='green')
					segments((i-iv-0.2)+0.5,llim[i],(i-iv+0.2)+0.5,llim[i],col='green')
					segments((i-iv-0.2)+0.5,ulim[i],(i-iv+0.2)+0.5,ulim[i],col='green')
				}
			}
		}else{
			tk_messageBox(type=c("ok"),message='Missing Y!',caption="Input Error")
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Evaluation First in DOE!',caption="Input Error")
	}
}
DOE_CVresiduals_experimental<-function(){
	if (exists('DOE',envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		if(DOE$loY){
			y<-DOE$y
			nr<-nrow(DOE$x)
			predcv<-DOE$predcv
			rescv<-DOE$rescv
			minval<-min(0,rescv)
			maxval<-max(0,rescv)
			dl<-c(minval-(maxval-minval)*0.05,maxval+(maxval-minval)*0.05)
			op<-par(pty='s',mfrow=c(1,2))
			plot(y,rescv,col='red',ylim=dl,type='p',xlab='Experimental Value',cex.main=0.8,
			ylab='Residual in CV',main='Experimental Values vs CV Residuals')
			abline(h=0,col='green',lty=2)
			grid()
			plot(1:nr,predcv-y,col='red',xlim=c(0.5,(nr+0.5)),type='p',xlab='Sample Number',cex.main=0.8,
			ylab='Residual in CV',main='CV Residuals for Experimental Points')
			abline(h=0,col='green',lty=2)
			grid()
			par(op)
		}else{
			tk_messageBox(type=c("ok"),message='Missing Y!',caption="Input Error")}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Evaluation First in DOE!',caption="Input Error")
	}
}
DOE_doptimal<-function(previous.name=''){
	# internal function definition
	matmod<-function(xexp,lI,lHT){
		tot<-xexp
		nr<-nrow(xexp)
		nc<-ncol(xexp)
		m<-matrix(rep(0,nc*nc),nc,nc)
		if(as.logical(lHT)){
			m<-matrix(rep(1,nc*nc),nc,nc)
			diag(m)<-rep(0,nc)
			m<-as.data.frame(m)
			names(m)<-names(xexp)
			row.names(m)<-names(m)
			m<-as.data.frame(upper.triangle(as.matrix(m)))
			m<-dfedit(m,dataset.name=deparse(substitute(items)),autosize=FALSE,
			size=c(110*nc,40*nc),editable=TRUE,update=TRUE,modal=TRUE)
		}
		coeff<-rep(0,nc+nc*(nc-1)/2+1)
		z<-0
		a<-nc
		for(j1 in 1:(nc-1)){
			for(j2 in (j1+1):nc){
				z<-z+1
				if(m[j1,j2]==1){
					coeff[z]<-coeff[z]+1
					a<-a+1
					tot<-cbind(tot,tot[,j1]*tot[,j2])
				}
			}
		}
		z<-nc*(nc-1)/2
		for(j1 in 1:nc){
			z<-z+1
			if(m[j1,j1]==1){
				coeff[z]<-coeff[z]+1
				a<-a+1
				tot<-cbind(tot,tot[,j1]^2)
			}
		}
		if(as.logical(lI)){
			coeff[nc+nc*(nc-1)/2+1]<-1
			a<-a+1
			tot<-cbind(rep(1,nr),tot)
		}
		tot<-as.data.frame(tot)
		names(tot)<-as.character(1:ncol(tot))
		rownames(tot)<-as.character(1:nrow(tot))
		return(tot)
	}
	# Menu
	ans<-inpboxek2(c('*Matrix with Candidate Points','Model with :','Intercept','Higher Terms'),c(previous.name,'TRUE','TRUE'))
	if(!is.null(ans)){
		name<-ans[[1]]
		x<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if((typeof(xexp)=='double')|(typeof(xexp)=='list')){
			x<-matmod(x,ans[[2]],ans[[3]])
			x<-as.matrix(x)
			maxinfl<-NULL
			r<-nrow(x)
			co<-ncol(x)
			print(paste('The Model has ',co,' coefficients',sep=''),quote=FALSE)
			ans<-inpboxe4(c('*Lower Number of Experiments','*Upper Number of Experiments','*Incremental Step','*Number of trials'),c(co,r-1,1,10))
			l<-as.numeric(ans[[1]])
			if(l<co){
				l<-co
				print(paste('You cannot have less experiments than coefficients - Lowest number of Experiments will be',co,sep=''),quote=FALSE)
			}
			h<-as.numeric(ans[[2]])
			if(h>(r-1)){
				h<-r-1
				print(paste('The Experimental Matrix must have a subset of the candidate points - Highest number of Experiments will be ',r-1,sep=''),quote=FALSE)
			}
			ne<-as.numeric(ans[[3]])
			nt<-as.numeric(ans[[4]])
			nlog<-seq(from=l,to=h,by=ne)
			expt<-matrix(0,length(nlog),h)
			logmt<-matrix(0,2,length(nlog))
			logmt[1,]<-nlog
			w<-0
			for(n in nlog){
				w<-w+1
				maxt<-0
				for(j in 1:nt){
					miss<-0
					dmax<-0
					xin<-NULL
					o<-randperm(1:r)
					while (miss<5){
						xin<-as.matrix(x[o[1:n],])
						xout<-as.matrix(x[o[(n+1):r],])
						if(ncol(xout)==1)xout<-t(xout)
						d<-det(t(xin)%*%xin)
						if(d>dmax){
							dmax<-d
							if(d>maxt){
								mexp<-o[1:n]
								maxt<-d
							}
						}else{
							miss<-miss+1
						}
						levin<-diag(xin%*%ginv(t(xin)%*%xin)%*%t(xin))
						levout<-diag(xout%*%ginv(t(xin)%*%xin)%*%t(xout))
						j1<-o[which.min(levin)]
						j2<-o[which.max(levout)+n]
						o[which.min(levin)]<-j2
						o[which.max(levout)+n]<-j1
					}
				}
				mexp<-mexp[order(mexp)]
				print('',quote=FALSE)
				print('Selected Points: ',quote=FALSE)
				print(as.vector(mexp))
				print(paste('Log(det):',log10(maxt)),quote=FALSE)
				logm<-log10(det(t(x[mexp,])%*%x[mexp,])/n^co)
				print(paste('Log(M):',logm),quote=FALSE)
				expt[w,1:n]<-mexp
				logmt[2,w]<-logm
				# computing Inflation factors
				sel<-as.matrix(x[mexp,])
				rsel<-nrow(sel)
				csel<-ncol(sel)
				xcc<-sel-matrix(1,rsel,1)%*%apply(sel,2,mean)
				infl<-apply((xcc^2),2,sum)*diag(inv(t(sel)%*%sel))
				maxinfl<-c(maxinfl,max(infl))
				print('Inflation Factors:')
				print(as.vector(infl),quote=FALSE)
				plot(logmt[1,1:w],logmt[2,1:w],col='red',type='b',xlab='Number of Experiments',
				ylab='log(Normalized Determinant)')
				grid()
			}
			win.graph()
			plot(logmt[1,1:w],maxinfl,ylim=c(min(maxinfl,4,8),max(maxinfl,4,8)),col='red',
			type='b',xlab='Number of Experiments',ylab='Maximum Inflation Factor')
			grid()
			abline(h=4,lty=2,col='red')
			abline(h=8,lty=2,col='red')
			assign('expt',expt,envir=.GlobalEnv)
			print('The matrix is saved in expt',quote=FALSE)
			print('Type expt on the console to see it',quote=FALSE)
		}
	}
}
DOE_doptadd<-function(previous.name=''){
	# internal function definition
	matmod<-function(xexp,lI,lHT){
		tot<-xexp
		nr<-nrow(xexp)
		nc<-ncol(xexp)
		m<-matrix(rep(0,nc*nc),nc,nc)
		if(as.logical(lHT)){
			m<-matrix(rep(1,nc*nc),nc,nc)
			diag(m)<-rep(0,nc)
			m<-as.data.frame(m)
			names(m)<-names(xexp)
			row.names(m)<-names(m)
			m<-as.data.frame(upper.triangle(as.matrix(m)))
			m<-dfedit(m,dataset.name=deparse(substitute(items)),autosize=FALSE,
			size=c(110*nc,40*nc),editable=TRUE,update=TRUE,modal=TRUE)
		}
		coeff<-rep(0,nc+nc*(nc-1)/2+1)
		z<-0
		a<-nc
		for(j1 in 1:(nc-1)){
			for(j2 in (j1+1):nc){
				z<-z+1
				if(m[j1,j2]==1){
					coeff[z]<-coeff[z]+1
					a<-a+1
					tot<-cbind(tot,tot[,j1]*tot[,j2])
				}
			}
		}
		z<-nc*(nc-1)/2
		for(j1 in 1:nc){
			z<-z+1
			if(m[j1,j1]==1){
				coeff[z]<-coeff[z]+1
				a<-a+1
				tot<-cbind(tot,tot[,j1]^2)
			}
		}
		if(as.logical(lI)){
			coeff[nc+nc*(nc-1)/2+1]<-1
			a<-a+1
			tot<-cbind(rep(1,nr),tot)
		}
		tot<-as.data.frame(tot)
		names(tot)<-as.character(1:ncol(tot))
		rownames(tot)<-as.character(1:nrow(tot))
		return(tot)
	}
	# Menu
	ans<-inpboxe2k2(c('*Matrix with Performed Experiments','*Matrix with Candidate Points','Intercept','Higher Terms'),c(previous.name,'','TRUE','TRUE'))
	if(!is.null(ans)){
		name<-ans[[1]]
		xori<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		name<-ans[[2]]
		x<-givemat(name)
		r<-nrow(x)
		co<-ncol(x)
		ror<-nrow(xori)
		tot<-rbind(xori,x)
		tot<-matmod(tot,ans[[3]],ans[[4]])
		maxinfl<-NULL
		rtot<-nrow(tot)
		co<-ncol(tot)
		xori<-as.matrix(tot[1:ror,])
		x<-as.matrix(tot[(ror+1):rtot,])
		print(paste('The Model has ',co,' coefficients',sep=''),quote=FALSE)
		nnt<-co-ror
		if(nnt<1)nnt<-1
		ans<-inpboxe4(c('*Lower Number of Experiments','*Upper Number of Experiments','*Incremental Step','*Number of trials'),c(nnt,r-1,1,10))
		l<-as.numeric(ans[[1]])
		if(l+ror<co){
			l<-co
			print(paste('You cannot have less experiments than coefficients - Lowest number of Experiments will be',co,sep=''),quote=FALSE)
		}
		h<-as.numeric(ans[[2]])
		ne<-as.numeric(ans[[3]])
		nt<-as.numeric(ans[[4]])
		nlog<-seq(from=l,to=h,by=ne)
		expt<-matrix(0,length(nlog),h)
		logmt<-matrix(0,2,length(nlog))
		logmt[1,]<-nlog
		w<-0
		for(n in nlog){
			w<-w+1
			maxt<-0
			for(j in 1:nt){
				miss<-0
				xin<-NULL
				dmax<-0
				o<-randperm(1:r)
				while (miss<5){
					xin<-x[o[1:n],]
					xin<-matrix(xin,nrow=n,ncol=ncol(x))
					xout<-as.matrix(x[o[(n+1):r],])
					if(ncol(xout)==1)xout<-t(xout)
					totin<-rbind(xor,xin)
					d<-det(t(totin)%*%totin)
					if(d>dmax){
						dmax<-d
						if(d>maxt){
							mexp<-o[1:n]
							maxt<-d
						}
					}else{
						miss<-miss+1
					}
					levin<-diag(xin%*%ginv(t(totin)%*%totin)%*%t(xin))
					levout<-diag(xout%*%ginv(t(totin)%*%totin)%*%t(xout))
					j1<-o[which.min(levin)]
					j2<-o[which.max(levout)+n]
					o[which.min(levin)]<-j2
					o[which.max(levout)+n]<-j1
				}
			}
			if(miss<05){
				mexp<-mexp[order(mexp)]
				print('',quote=FALSE)
				print('Selected Points: ',quote=FALSE)
				print(as.vector(mexp))
				print(paste('Log(det):',log10(maxt)),quote=FALSE)
				totin<-rbind(xori,x[mexp,])
				logm<-log10(det(t(totin)%*%totin)/nrow(totin)^co)
				print(paste('Log(M):',logm),quote=FALSE)
				expt[w,1:n]<-mexp
				logmt[2,w]<-logm
	#	       computing inflation factors
				sel<-as.matrix(totin)
				rsel<-nrow(sel)
				csel<-ncol(sel)
				xcc<-sel-matrix(1,rsel,1)%*%apply(sel,2,mean)
				infl<-apply((xcc^2),2,sum)*diag(pinv(t(sel)%*%sel))
				maxinfl<-c(maxinfl,max(infl))
				print('Inflation Factors:',quote=FALSE)
				print(as.vector(infl),quote=FALSE)
				plot(logmt[1,1:w],logmt[2,1:w],col='red',type='b',xlab='Number of Additional Experiments',ylab='log(Normalized Determinant)')
				grid()
			}else{
				print(miss)
				print('The matrix is almost singular, I cannot evaluate the determinant.',quote=FALSE)
				break
			}
		}
		if(miss<=5){
			win.graph()
			plot(logmt[1,1:w],maxinfl,ylim=c(1,max(maxinfl,8)),col='red',
			type='b',xlab='Number of Experiments',ylab='Maximum Inflation Factor')
			grid()
			abline(h=4,lty=2,col='red')
			abline(h=8,lty=2,col='red')
			assign('expt',expt,envir=.GlobalEnv)
			print('The matrix is saved in expt',quote=FALSE)
			print('Type expt on the console to see it',quote=FALSE)
		}
	}
}
DOE_experimental_fitted<-function(){
	if(exists('DOE',envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		if(DOE$loY){
			y<-DOE$y
			nr<-nrow(DOE$x)
			pred<-DOE$pred
			minval<-min(y,pred)
			maxval<-max(y,pred)
			dl<-c(minval-(maxval-minval)*0.05,maxval+(maxval-minval)*0.05)
			plot(y,pred,type='n',col='red',xlim=dl,ylim=dl,xlab='Experimental Value',
			ylab='Fitted Value',main='Experimental vs. Fitted Values')
			abline(a=0,b=1,col='green',lty=1)
			grid()
			for(i in 1:nr){
				text(y[i],pred[i],as.character(i),col='red')
			}
		}else{
			tk_messageBox(type=c("ok"),message='Missing Y!',caption="Input Error")}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Evaluation First in DOE!',caption="Input Error")
	}
}
DOE_experimental_predicted<-function(){
	if(exists('DOE',envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		if(DOE$loY){
			y<-DOE$y
			nr<-nrow(DOE$x)
			predcv<-DOE$predcv
			minval<-min(y,predcv)
			maxval<-max(y,predcv)
			dl<-c(minval-(maxval-minval)*0.05,maxval+(maxval-minval)*0.05)
			plot(y,predcv,type='n',col='red',xlim=dl,ylim=dl,xlab='Experimental Value',
			ylab='CV Predicted Value',main='Experimental versus CV Predicted Values')
			for(i in 1:nr){
				text(y[i],predcv[i],as.character(i),col='red')
			}
			abline(a=0,b=1,col='green',lty=1)
			grid()
		}else{
			tk_messageBox(type=c("ok"),message='Missing Y!',caption="Input Error")}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Evaluation First in DOE!',caption="Input Error")
	}
}
DOE_leverage_surface<-function(){
	if(exists('DOE',envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		ans<-inpboxe2(c('*Minimum value of range','*Maximum value of range'),c('',''))
		if(!is.null(ans)){
			minrange<-as.numeric(ans[[1]])
			maxrange<-as.numeric(ans[[2]])
			nv<-DOE$nv
			coeff<-DOE$coeff
			disper<-DOE$disper
			z<-0
			nstep<-30
			st<-(maxrange-minrange)/nstep
			lab<-seq(minrange,maxrange,by=st)
			r<-(nstep+1)^2
			c<-nv
			gr<-matrix(0,r,c)
			ans<-inpboxc2(c('Index of the variable on X-axis','Index of the variable on Y-axis'),as.character(1:nv),as.character(1:nv))
			if(!is.null(ans)){
				v1<-as.numeric(ans[[1]])
				v2<-as.numeric(ans[[2]])
				a<-0;x<-NULL
				for(ij in seq(minrange,maxrange,by=st)){
					for(y in seq(minrange,maxrange,by=st)){
						a<-a+1
						gr[a,v1]=ij
						gr[a,v2]=y
					}
				}
				for(i in 1:nv){
					if((i!=v1)&(i!=v2)){
						ans<-inpboxe1(paste('Value of variable ',i,' ? ',sep=''),'')
						x<-as.numeric(ans[[1]])
						if(x!=0){
							gr[,i]<-gr[,i]+x
						}
					}
				}
				a<-c
				for(j1 in 1:(c-1)){
					for(j2 in (j1+1):c){
						z<-z+1
						if(coeff[z]==1){
							a<-a+1
							gr<-cbind(gr,gr[,j1]*gr[,j2])
						}
					}
				}
				for(j1 in 1:c){
					z<-z+1
					if(coeff[z]==1){
						a<-a+1
						gr<-cbind(gr,gr[,j1]^2)
					}
				}
				z<-z+1
				if(coeff[z]==1){
					a<-a+1
					gr<-cbind(rep(1,r),gr)
				}
				ir<-1
				lev<-matrix(0,nstep+1,nstep+1)
				for(j in 1:(nstep+1)){
					lev[,j]<-diag(gr[(ir:(ir+nstep)),]%*%disper%*%t(gr[(ir:(ir+nstep)),]))
					ir<-ir+nstep+1
				}
				print(paste('Leverage: min. ',format(min(lev),digits=4),' average ',
				format(mean(lev),digits=4),' max. ',format(max(lev),digits=4),sep=''))
				win.graph()
				zlab<-NULL
				if(!is.null(x))zlab<-paste('Resp.at',format(x,digits=4))
				lev<-t(lev)     # necessary to make plot consistent with X-Y choice
				persp(lab,lab,lev,main='Plot of Leverage',cex.main=0.8,xlab=paste('Var.n.',v1),ylab=paste('Var.n.',v2),zlab=zlab)
				win.graph()
				contour(lab,lab,lev,nlevels=10,main='Plot of Leverage - Contour Plot',cex.main=0.8,xlab=paste('Var.n.',v1),ylab=paste('Var.n.',v2))
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Computation First in DOE!',caption="Input Error")
	}
}
DOE_model_computation<-function(previous.name=''){
    if(exists('DOE',envir=.GlobalEnv))rm('DOE',envir=.GlobalEnv)
	if(!exists('doe.set',envir=.GlobalEnv))doe.set<-c(previous.name,'all','all','None','TRUE','TRUE')
	ans<-inpboxe4k2(c('*Matrix Name','*Rows to be selected (e.g., 1:10,15)','*X-Variables to be selected (e.g., 1:4,8)',
	'Y-Variable to be selected (e.g., 9)','Intercept','Higher Terms'),doe.set)
	if(!is.null(ans)){
		DOE<-list()
		name<-ans[[1]]
		doe.set<-ans
		M<-givemat(name)
		loY<-TRUE
		if(ans[[4]]!='None'){
			if(is.na(as.numeric(ans[[4]]))){
				Y<-givemat(ans[[4]],nl=as.numeric(ans[[2]]))
			}else{
				Y<-M[,as.numeric(ans[[4]])] 
				if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
			}
		}else{
			loY<-FALSE
			Y<-rep(0,as.numeric(ans[[2]]))
			if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
		}
		assign('previous.name',name,envir=.GlobalEnv)
		if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
		if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
		if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
		if((typeof(M)=='double')|(typeof(M)=='list')){
			M<-data.frame(cbind(Y,M))
			naM<-names(M)
			naM<-naM[-1]
			nr<-nrow(M)
			nc<-ncol(M)-1
			#
			# build system model matrices
			#
			x<-M[,-1]
			y<-M[,1]
			m<-matrix(0,nc,nc)
			if(as.logical(ans[[6]])){
				m<-matrix(rep(1,nc*nc),nc,nc)
				diag(m)<-rep(0,nc)
				m<-as.data.frame(m)
				names(m)<-naM
				row.names(m)<-names(m)
				m<-as.data.frame(upper.triangle(as.matrix(m)))
				m<-dfedit(m,dataset.name=deparse(substitute(items)),autosize=FALSE,
				size=c(110*nc,40*nc),editable=TRUE,update=TRUE,modal=TRUE)
			}
			coeff<-rep(0,nc+nc*(nc-1)/2+1)
			z<-0
			a<-nc
			for(j1 in 1:(nc-1)){
				for(j2 in (j1+1):nc){
					z<-z+1
					if(m[j1,j2]==1){
						coeff[z]<-coeff[z]+1
						a<-a+1
						x<-cbind(x,x[,j1]*x[,j2])
					}
				}
			}
			z<-nc*(nc-1)/2
			for(j1 in 1:nc){
				z<-z+1
				if(m[j1,j1]==1){
					coeff[z]<-coeff[z]+1
					a<-a+1
					x<-cbind(x,x[,j1]^2)
				}
			}
			if(as.logical(ans[[5]])){
				coeff[nc+nc*(nc-1)/2+1]<-1
				a<-a+1
				x<-cbind(rep(1,nr),x)
			}
			x<-as.matrix(x)
			ncx<-ncol(x)
			#
			# solve the system
			#
			print('',quote=FALSE)
			print('*************************  Model Solution  *******************************',quote=FALSE)
			print('',quote=FALSE)
			inf<-t(x)%*%x
			disper<-ginv(inf)
			print('',quote=FALSE)
			print('Dispersion Matrix',quote=FALSE)
			print.table(format(disper,digit=3,scientific =FALSE),quote=FALSE)
			tr<-matrix.trace(disper)
			print('',quote=FALSE)
			print('Trace',quote=FALSE)
			print(format(tr,digit=4),quote=FALSE)
			xcc<-x-matrix(rep(1,nr),nr,1)%*%apply(x,2,'mean')
			inf1<-apply(xcc^2,2,sum)*diag(disper)
			print('',quote=FALSE)
			print('Inflation Factors',quote=FALSE)
			print.table(format(as.vector(inf1),digit=4),quote=FALSE)
			print('',quote=FALSE)
			print('Leverage of the Experimental Points',quote=FALSE)
			lev<-diag(x%*%disper%*%t(x))
			print(format(as.vector(t(lev)),digit=4),quote=FALSE)
			print('',quote=FALSE)
			print('Maximum leverage',quote=FALSE)
			print(format(max(lev),digit=4),quote=FALSE)
			if(loY){
				b<-disper%*%t(x)%*%y
				print('',quote=FALSE)
				print('Coefficients',quote=FALSE)
				print(format(as.vector(b),digit=4),quote=FALSE)
				dof<-nr-ncx
				print('',quote=FALSE)
				print('Degrees of freedom',quote=FALSE)
				print(format(dof,digit=4),quote=FALSE)
				if(dof>0){
					pred<-x%*%b
					varres<-sum((y-pred)^2)/dof
					rmsef<-sqrt(varres)
					varcoeff<-varres*diag(disper)
					sdcoeff<-sqrt(varcoeff)
					print('',quote=FALSE)
					print('Std.dev. of coefficients:',quote=FALSE)
					print(format(sdcoeff,digit=4),quote=FALSE)
					print('',quote=FALSE)
					print('Significance of the coefficients',quote=FALSE)
					t<-abs(b/sdcoeff)
					sig<-(1-pt(t,dof))*2
					print(format(as.vector(sig),digit=4),quote=FALSE)
					print('',quote=FALSE)
					print('Fitted Values',quote=FALSE)
					print(format(as.vector(pred),digit=4),quote=FALSE)
					print('',quote=FALSE)
					print('Residuals',quote=FALSE)
					print(format(as.vector(pred-y),digit=4),quote=FALSE)
					print('',quote=FALSE)
					print('Variance of Y',quote=FALSE)
					vary<-sd(y)^2
					print(format(vary,digit=4),quote=FALSE)
					print('',quote=FALSE)
					print('Standard Deviation',quote=FALSE)
					print(format(rmsef,digit=4),quote=FALSE)
					print('',quote=FALSE)
					print('% Explained variance',quote=FALSE)
					print(format((1-varres/vary)*100,digit=4),quote=FALSE)
					predcv<-rep(0,nr)
					bcr<-matrix(0,nr,ncx)
					for(i in 1:nr){
						xcv<-x[-i,]
						ycv<-y[-i]
						bcv<-ginv(t(xcv)%*%xcv)%*%t(xcv)%*%ycv
						bcr[i,]<-t(bcv)
						predcv[i]<-x[i,]%*%bcv
					}
					print('',quote=FALSE)
					print('CV Values',quote=FALSE)
					print(format(predcv,digit=4),quote=FALSE)
					print('CV Residuals',quote=FALSE)
					rescv<-predcv-y
					print(format(as.vector(rescv),digit=4),quote=FALSE)
					varrescv=sum((y-predcv)^2)/nr
					rmsecv<-sqrt(varrescv)
					print('',quote=FALSE)
					print('RMSECV',quote=FALSE)
					print(format(rmsecv,digit=4),quote=FALSE)
					print('',quote=FALSE)
					print('% CV Explained Variance',quote=FALSE)
					print(format((1-varrescv/vary)*100,digit=4),quote=FALSE)
					bmat<-t(b%*%matrix(1,1,nr))
					res<-(bcr-bmat)^2
					print('',quote=FALSE)
					print('Std.dev. of the coefficients according to resampling',quote=FALSE)
					sdres<-sqrt(apply(res,2,sum)*nr/(nr-1))
					print(format(sdres,digit=4),quote=FALSE)
					print('',quote=FALSE)
					print('Significance of the coefficients according to resampling',quote=FALSE)
					t<-abs(b/sdres)
					print(format((as.vector(1-pt(t,nr))*2),digit=4),quote=FALSE)
				}
				if(dof==0){
					print('0 Degrees of Freedom: no diagnostic plots allowed',quote=FALSE)
				}
				if(dof<0){
					print('Negative Degree of Freedom: Calculation Ends',quote=FALSE)
				}
			}
			print('',quote=FALSE)
			print('**************************************************************************',quote=FALSE)
			#
			# save results in DOE object
			#
            DOE$name<-naM
			DOE$x<-x
			DOE$y<-y
			DOE$nv<-nc
			DOE$m<-m
			DOE$coeff<-coeff
			DOE$inf<-inf
			DOE$disper<-disper
			DOE$tr<-tr
			DOE$lev<-lev
			DOE$loY<-loY
			if(loY){
				DOE$b<-b
				DOE$dof<-dof
				if(dof>0){
					DOE$pred<-pred
					DOE$varres<-varres
					DOE$rmsef<-rmsef
					DOE$varcoeff<-varcoeff
					DOE$sig<-sig
					DOE$vary<-vary
					DOE$predcv<-predcv
					DOE$rescv<-rescv
					DOE$varrescv<-varrescv
					DOE$rmsecv<-rmsecv
					DOE$sdres<-sdres
					DOE$sdcoeff<-sdcoeff
				}
			}
			assign('DOE',DOE,envir=.GlobalEnv)
			assign('doe.set',doe.set,envir=.GlobalEnv)
		}
	}
}
DOE_extract<-function(){
	if(exists("DOE",envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		ans<-inpboxc('Extract Matrix:',c('Dispersion Matrix','Coefficients','Fitted Values','CV predicted','CV Residuals'))
		if(!is.null(ans)){
			if(ans[[1]]==1){
				DMdoe<-DOE$disper
				assign('DMdoe',DMdoe,envir=.GlobalEnv)
				print('Values saved in DMdoe matrix',quote=FALSE)
			}
			if(ans[[1]]==2){
				Bdoe<-t(DOE$b)
				assign('Bdoe',Bdoe,envir=.GlobalEnv)
				print('Values saved in Bdoe vector',quote=FALSE)
			}
			if(ans[[1]]==3){
				FTdoe<-t(DOE$pred)
				assign('FTdoe',FTdoe,envir=.GlobalEnv)
				print('Values saved in FTdoe vector',quote=FALSE)
			}
			if(ans[[1]]==4){
				PDdoe<-t(DOE$predcv)
				assign('PDdoe',PDdoe,envir=.GlobalEnv)
				print('Values saved in PDdoe vector',quote=FALSE)
			}
			if(ans[[1]]==5){
				RSdoe<t(DOE$rescv)
				assign('RSdoe',RSdoe,envir=.GlobalEnv)
				print('Values saved in RSdoe vector',quote=FALSE)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Evaluation First in DOE!',caption="Input Error")
	}
}
DOE_prediction<-function(previous.name=''){
	if (exists('DOE',envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		if(DOE$loY){
			ans<-inpboxe4(c('*Matrix Name with experiments to be predicted','*Rows to be selected (e.g., 1:10,15)',
			'*X-Variables to be selected (e.g., 1:4,8)','Y-Variable to be selected (e.g., 9)'),c(previous.name,'all','','None'))
			if(!is.null(ans)){
				name<-ans[[1]]
				M<-givemat(name)
				assign('previous.name',name,envir=.GlobalEnv)
				loY<-TRUE
				if(ans[[4]]!='None'){
					Y<-M[,as.integer(ans[[4]])]
				}else{
					loY<-FALSE
					Y<-rep(0,nrow(M))
				}
				if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
				if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
				if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
				if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
				if((typeof(M)=='double')|(typeof(M)=='list')){
					M<-data.frame(cbind(Y,M))
					naM<-names(M)
					naM<-naM[-1]
					nr<-nrow(M)
					nc<-ncol(M)-1
					nv<-DOE$nv
					b<-DOE$b
					m<-DOE$m
					coeff<-DOE$coeff
					if(nv==nc){
						x<-M[,-1]
						y<-M[,1]
						z<-0
						a<-nc
					for(j1 in 1:(nc-1)){
						for(j2 in (j1+1):nc){
							z<-z+1
							if(m[j1,j2]==1){
								a<-a+1
								x<-cbind(x,x[,j1]*x[,j2])
							}
						}
					}
					z<-nc*(nc-1)/2
					for(j1 in 1:nc){
						z<-z+1
						if(m[j1,j1]==1){
							a<-a+1
							x<-cbind(x,x[,j1]^2)
						}
					}
					if(coeff[length(coeff)]==1){
						a<-a+1
						x<-cbind(rep(1,nr),x)}
						var.fitted<-as.matrix(x)%*%b
						print(var.fitted)
						print('The value is saved in: var.fitted')
						if(loY){
							op<-par(pty='s',mfrow=c(1,2))
							xl<-c(min(min(y),min(var.fitted),min(DOE$predcv)),max(max(y),max(var.fitted),max(DOE$predcv)))
							yl<-xl
							plot(y,var.fitted,xlab='Experimental Value',ylab='Predicted Value',asp=1,xlim=xl,ylim=yl)
							lines(par('usr')[1:2],par('usr')[3:4],col='red')
							grid()
							yl<-c(min((var.fitted-y),min(DOE$predcv-DOE$y)),max(max(var.fitted-y),max(DOE$predcv-DOE$y)))
							plot(1:nr,var.fitted-y,xlab='Object Number',ylab='Residuals',ylim=yl)
							abline(h=0,col="red")
							grid()
							par(op)
						}
					}	
				}
			}else{
				tk_messageBox(type=c("ok"),message='Wrong dimension in new vector !',caption="Input Error")
			}
		}else{
			tk_messageBox(type=c("ok"),message='Missing Y!',caption="Input Error")
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Evaluation First in DOE!',caption="Input Error")
	}
}
DOE_residuals_fitting<-function(){
	if(exists('DOE',envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		if(DOE$loY){
			y<-DOE$y
			nr<-nrow(DOE$x)
			pred<-DOE$pred
			plot(1:nr,pred-y,col='red',xlim=c(0.5,(nr+0.5)),type='p',xlab='Sample Number',
			ylab='Residual in Fitting',main='Residuals in Fitting')
			abline(h=0,col='green',lty=2)
			grid()
		}else{
			tk_messageBox(type=c("ok"),message='Missing Y!',caption="Input Error")
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Evaluation First in DOE!',caption="Input Error")
	}
}
DOE_response_surface<-function(){
	if(exists('DOE',envir=.GlobalEnv)){
		get("DOE",envir=.GlobalEnv)
		if(DOE$loY){
			ans<-inpboxe2(c('*Minimum value of range','*Maximum value of range'),c('',''))
			if(!is.null(ans)){
				minrange<-as.numeric(ans[[1]])
				maxrange<-as.numeric(ans[[2]])
				nv<-DOE$nv
				coeff<-DOE$coeff
				b<-DOE$b
				z<-0
				nstep<-30
				st<-(maxrange-minrange)/nstep
				lab<-seq(minrange,maxrange,by=st)
				r<-(nstep+1)^2
				c<-nv
				gr<-matrix(0,r,c)
				ans<-inpboxc2(c('*Index of the variable on X-axis','*Index of the variable on Y-axis'),as.character(1:nv),as.character(1:nv))
				if(!is.null(ans)){
					v1<-as.numeric(ans[[1]])
					v2<-as.numeric(ans[[2]])
					a<-0
					x<-NULL
					for(ij in seq(minrange,maxrange,by=st)){
						for(y in seq(minrange,maxrange,by=st)){
							a<-a+1
							gr[a,v1]=ij
							gr[a,v2]=y
						}
					}
					for(i in 1:nv){
						if((i!=v1)&(i!=v2)){
							ans<-inpboxe1(paste('*Value of variable ',i,' ? ',sep=''),'')
							if(!is.null(ans)){
								x<-as.numeric(ans[[1]])
								if(x!=0){
									gr[,i]<-gr[,i]+x
								}
							}
						}
					}
					a<-c
					for(j1 in 1:(c-1)){
						for(j2 in (j1+1):c){
							z<-z+1
							if(coeff[z]==1){
								a<-a+1
								gr<-cbind(gr,gr[,j1]*gr[,j2])
							}
						}
					}
					for(j1 in 1:c){
						z<-z+1
						if(coeff[z]==1){
							a<-a+1
							gr<-cbind(gr,gr[,j1]^2)
						}
					}
					z<-z+1
					if(coeff[z]==1){
						a<-a+1
						gr<-cbind(rep(1,r),gr)
					}
					ir<-1
					risp<-matrix(0,nstep+1,nstep+1)
					for(j in 1:(nstep+1)){
						risp[,j]<-gr[(ir:(ir+nstep)),]%*%b
						ir<-ir+nstep+1
					}
					win.graph()
					risp<-t(risp)                   # necessary to make plot consistent with X-Y choice
					if(abs((max(risp)-min(risp))/max(risp))>0.01){
						persp(lab,lab,risp,main='Response Surface',cex.main=0.8,xlab=DOE$name[v1],
						ylab=DOE$name[v2],zlab=paste('Response'),col='red')
						win.graph()
						contour(lab,lab,risp,nlevels=10,main='Response Surface: Contour Plot',cex.main=0.8,
						xlab=DOE$name[v1],ylab=DOE$name[v2],col='blue')
					}else{
						print('3D plot impossible: third variable apparently constant')
					}
				}
			}
		}else{
			tk_messageBox(type=c("ok"),message='Missing Y!',caption="Input Error")
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model Computation First in DOE!',caption="Input Error")
	}
}
W3_extract<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		ans<-inpboxc('*Extract Matrix:',c('Object Loadings','Variables Loadings','Conditons Loadings'))
		if(!is.null(ans)){
			if(ans[[1]]==1){
				lo<-PCA3W$lo
				assign('lo',lo,envir=.GlobalEnv)
				print('Values saved in lo matrix',quote=FALSE)}
			if(ans[[1]]==2){
				lv<-PCA3W$lv
				assign('lv',lv,envir=.GlobalEnv)
				print('Values saved in lv matrix',quote=FALSE)
			}
			if(ans[[1]]==3){
				lc<-PCA3W$lc
				assign('lc',lc,envir=.GlobalEnv)
				print('Values saved in lc matrix',quote=FALSE)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
W3_model<-function(previous.name=''){
	if(exists('PCA3W',envir=.GlobalEnv))rm('PCA3W',envir=.GlobalEnv)
	ans<-inpboxe2c(c('*Matrix Name','*Number of conditions','Scaling Method'),c('none','j-scaling','jk-scaling'),
	c(previous.name,'',''))
	if(!is.null(ans)){
		PCA3W<-list()
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if((typeof(M)=='double')|(typeof(M)=='list')){
			M<-as.matrix(M)
			Mo<-NULL
			Mv<-NULL
			Mc<-NULL
			if(as.numeric(ans[[3]])==2)M<-scale(M,center=TRUE,scale=TRUE)
			oc<-nrow(M)
			v<-ncol(M)
			c<-as.numeric(ans[[2]])
			maxci<-20 # fix number of maximum iterations
			o<-oc/c
			for(i in 1:c)Mo<-cbind(Mo,M[(i-1)*o+1:o,])
			if(as.numeric(ans[[3]])==3){
				Mo<-scale(Mo,center=TRUE,scale=TRUE)
				M<-NULL
				for(i in 1:c)M<-rbind(M,Mo[,(i-1)*v+1:v])
			}
			for(i in 1:c)Mv<-cbind(Mv,t(Mo[,(i-1)*v+1:v]))
			for(i in 1:c){
				Mt<-NULL
				for(j in 1:o)Mt<-cbind(Mt,t(Mv[,(i-1)*o+j]))
				Mc<-rbind(Mc,Mt)
			}
			varin<-sum(sum(M^2))
			print('Note : Two Components per Mode will be computed',quote=FALSE)
			#print(paste('Initial variance',varin),quote=FALSE)
			res<-pca(t(Mo),method="nipals",nPcs=2,scale="none",center=FALSE)
			lo<-t(loadings(res))
			res<-pca(t(Mv),method="nipals",nPcs=2,scale="none",center=FALSE)
			lv<-t(loadings(res))
			res<-pca(t(Mc),method="nipals",nPcs=2,scale="none",center=FALSE)
			lc<-t(loadings(res))
			delta<-1
			ci<-0
			perc<-NULL
			while((delta>0.001)&(ci<maxci+1)){
				if (ci>0){
					m<-Mo%*%((t(lc)%*%lc)%x%(t(lv)%*%lv))%*%t(Mo)
					lo<-t(m%*%t(lo)%*%sqrtm(matrix.inverse(lo%*%m%*%m%*%t(lo))))
					m<-Mv%*%((t(lc)%*%lc)%x%(t(lo)%*%lo))%*%t(Mv)
					lv<-t(m%*%t(lv)%*%sqrtm(matrix.inverse(lv%*%m%*%m%*%t(lv))))
					m<-Mc%*%((t(lo)%*%lo)%x%(t(lv)%*%lv))%*%t(Mc)
					lc<-t(m%*%t(lc)%*%sqrtm(matrix.inverse(lc%*%m%*%m%*%t(lc))))
				}
				G<-lo%*%Mo%*%(t(lc)%x%t(lv))
				nG<-which.max(apply(abs(t(G)),2,max))
				if(nG==2){
					lo2=lo
					lo2[1,]<-lo[2,]
					lo2[2,]<-lo[1,]
					lo<-lo2
					G<-lo%*%Mo%*%(t(lc)%x%t(lv))
				}
				nG<-which.max(apply(abs(G),2,max))
				if((nG==2)|(nG==4)){
					lc2<-lc
					lc2[1,]<-lc[2,]
					lc2[2,]<-lc[1,]
					lc<-lc2
					G<-lo%*%Mo%*%(t(lc)%x%t(lv))
				}
				if(nG>2){
					lv2<-lv
					lv2[1,]<-lv[2,]
					lv2[2,]<-lv[1,]
					lv<-lv2
				}
				#  print(ci)
				G<-lo%*%Mo%*%(t(lc)%x%t(lv))
				percvarexp<-sum(sum(G^2))/varin*100
				#  print.table(G,quote=FALSE)
				#  print(paste('% expl. var.:',percvarexp),quote=FALSE)
				perc<-c(perc,percvarexp)
				if(ci>0)delta<-perc[ci+1]-perc[ci]
				ci<-ci+1
			}
			print('',quote=FALSE)
			print('Core Matrix',quote=FALSE)
			print('      ,1,1      ,2,1      ,1,2      ,2,2',quote=FALSE)
			print(paste('1,,',format(G[1,1],digit=5),format(G[1,2],digit=5),format(G[1,3],digit=5),
			format(G[1,4],digit=5),sep='   '),quote=FALSE)
			print(paste('2,,',format(G[2,1],digit=5),format(G[2,2],digit=5),format(G[2,3],digit=5),
			format(G[2,4],digit=5),sep='   '),quote=FALSE)
			print('',quote=FALSE)
			G2<-matrix(rep(0,8),2,4)
			fi<-(G[1,1]^2+G[2,4]^2)/sum(sum(G^2))
			fin<-0
			fimax<-fi
			while(fimax>fin){
				fin<-fimax
				# print('Rotation of objects',quote=FALSE)
				rotb<-0
				for(rot in -90:90){
					RM<-matrix(c(cos(rot*pi/180),sin(rot*pi/180),-sin(rot*pi/180),cos(rot*pi/180)),2,2)
					lo2<-RM%*%lo
					G2<-lo2%*%Mo%*%(t(lc)%x%t(lv))
					fi2<-(G2[1,1]^2+G2[2,4]^2)/sum(sum(G2^2))
					if(fi2>fimax){
						lob<-lo2
						fimax<-fi2
						rotb<-rot
					}
				}
				if(rotb!=0){
				#  print(rotb,quote=FALSE)
				#  print(fimax,quote=FALSE)
				lo<-lob}
				#  print('Rotation of variables',quote=FALSE)
				rotb<-0
				for(rot in -90:90){
					RM<-matrix(c(cos(rot*pi/180),sin(rot*pi/180),-sin(rot*pi/180),cos(rot*pi/180)),2,2)
					lv2<-RM%*%lv
					G2<-lo%*%Mo%*%(t(lc)%x%t(lv2))
					fi2<-(G2[1,1]^2+G2[2,4]^2)/sum(sum(G2^2))
					if(fi2>fimax){
						lvb<-lv2
						fimax<-fi2
						rotb<-rot
					}
				}
				if(rotb!=0){
					#   print(rotb,quote=FALSE)
					#   print(fimax,quote=FALSE)
					lv<-lvb
				}
				#  print('Rotation of conditions',quote=FALSE)
				rotb<-0
				for(rot in -90:90){
					RM<-matrix(c(cos(rot*pi/180),sin(rot*pi/180),-sin(rot*pi/180),cos(rot*pi/180)),2,2)
					lc2<-RM%*%lc
					G2<-lo%*%Mo%*%(t(lc2)%x%t(lv))
					fi2<-(G2[1,1]^2+G2[2,4]^2)/sum(sum(G2^2))
					if(fi2>fimax){
						lcb<-lc2
						fimax<-fi2
						rotb<-rot
					}
				}
				if(rotb!=0){
				#   print(rotb,quote=FALSE)
				#   print(fimax,quote=FALSE)
					lc<-lcb
				}
			}
			nc<-nrow(lo)
			for(k in 1:nc){
				mm<-mean(lo[k,])
				mt<-0
				for(i in 1:v){
					ml<-0
					for(j in 1:o)ml=ml+mean(M[seq(j,oc,o),i])%*%(lo[k,j]-mm)
					mt=mt+ml%*%lv[k,i]
				}
				if(mt<0)lo[k,]<--lo[k,]
				mm<-mean(lc[k,])
				mt<-0
				for(i in 1:v){
					ml<-0
					for(j in 1:c){
						ml<-ml+mean(M[((j-1)*o+1):(j*o),i])%*%(lc[k,j]-mm)
					}
					mt<-mt+ml%*%lv[k,i]
				}
				if(mt<0)lc[k,]<--lc[k,]
			}
			G<-lo%*%Mo%*%(t(lc)%x%t(lv))
			nG<-which.max(max(abs(t(G))))
			if(nG==2){
				lo2<-lo
				lo2[1,]<-lo[2,]
				lo2[2,]<-lo[1,]
				lo<-lo2
				G<-lo%*%Mo%*%(t(lc)%x%t(lv))
			}
			nG<-which.max(max(abs(G)))
			if((nG==2)|(nG==4)){
				lc2<-lc
				lc2[1,]<-lc[2,]
				lc2[2,]<-lc[1,]
				lc<-lc2
			}
			if(nG>2){
				lv2<-lv
				lv2[1,]<-lv[2,]
				lv2[2,]<-lv[1,]
				lv<-lv2
			}
			print('Superdiagonalized Core Matrix',quote=FALSE)
			print('      ,1,1      ,2,1      ,1,2      ,2,2',quote=FALSE)
			print(paste('1,,',format(G[1,1],digit=5),format(G[1,2],digit=5),format(G[1,3],digit=5),
			format(G[1,4],digit=5),sep='   '),quote=FALSE)
			print(paste('2,,',format(G[2,1],digit=5),format(G[2,2],digit=5),format(G[2,3],digit=5),
			format(G[2,4],digit=5),sep='   '),quote=FALSE)
			RMM<-matrix(rep(0,o*c*v),o*c,v)
			for(i in 1:o){
				for(j in 1:v){
					for(k in 1:c){
						rv<-   lo[1,i]%*%lv[1,j]%*%lc[1,k]%*%G[1,1]
						rv<-rv+lo[1,i]%*%lv[2,j]%*%lc[1,k]%*%G[1,2]
						rv<-rv+lo[1,i]%*%lv[1,j]%*%lc[2,k]%*%G[1,3]
						rv<-rv+lo[1,i]%*%lv[2,j]%*%lc[2,k]%*%G[1,4]
						rv<-rv+lo[2,i]%*%lv[1,j]%*%lc[1,k]%*%G[2,1]
						rv<-rv+lo[2,i]%*%lv[2,j]%*%lc[1,k]%*%G[2,2]
						rv<-rv+lo[2,i]%*%lv[1,j]%*%lc[2,k]%*%G[2,3]
						rv<-rv+lo[2,i]%*%lv[2,j]%*%lc[2,k]%*%G[2,4]
						RMM[o*(k-1)+i,j]<-rv
					}
				}
			}
			dis<-(RMM-M)^2
			# saving data
			PCA3W$dataset<-M
			PCA3W$limits<-c(c,o,v)
			PCA3W$G<-G
			PCA3W$lo<-lo
			PCA3W$lv<-lv
			PCA3W$lc<-lc
			PCA3W$var<-perc
			PCA3W$dis<-dis
			assign('PCA3W',PCA3W,envir=.GlobalEnv)			
		}else{
			tk_messageBox(type=c("ok"),message='Wrong Dataset Dimension!',caption="Input Error")
		}
	}
}
W3_plot_conditions<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		lc<-t(PCA3W$lc)
		ldim<-c(min(lc,0),max(lc,0))
		plot(lc[,1],lc[,2],type='n',main='Plot of Conditions',xlim=ldim,ylim=ldim,xlab='Axis 1',ylab='Axis 2')
		grid()
		text(lc[,1],lc[,2],as.character(1:PCA3W$limits[1]),col='blue',cex=0.8)
		points(0,0,pch='+',col='black')
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
W3_plot_objects<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		lo<-t(PCA3W$lo)
		ldim<-c(min(lo,0),max(lo,0))
		plot(lo[,1],lo[,2],type='n',main='Plot of Objects',xlim=ldim,ylim=ldim,xlab='Axis 1',ylab='Axis 2')
		grid()
		text(lo[,1],lo[,2],as.character(1:PCA3W$limits[2]),col='red',cex=0.8)
		points(0,0,pch='+',col='black')
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
W3_plot_rmse_conditions<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		c<-PCA3W$limits[1]
		o<-PCA3W$limits[2]
		v<-PCA3W$limits[3]
		rc<-rep(0,c)
		roc<-apply(PCA3W$dis,1,sum)
		for(i in 1:c)rc[i]<-sum(roc[((i-1)*o+1):(i*o)])
		rc<-rc/(o*v)
		barplot(rc,col='blue',main='RMSE of Conditions',ylim=c(0,1.2*max(rc)),names.arg=as.character(1:c),cex.names=0.6)
		box()
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
W3_plot_rmse_objects<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		c<-PCA3W$limits[1]
		o<-PCA3W$limits[2]
		v<-PCA3W$limits[3]
		ro<-rep(0,o)
		roc<-apply(PCA3W$dis,1,sum)
		for(i in 1:o)ro[i]<-sum(roc[seq(i,o*c,o)])
		ro<-ro/(v*c)
		barplot(ro,col='red',main='RMSE of Objects',ylim=c(0,1.2*max(ro)),names.arg=as.character(1:o),cex.names=0.6)
		box()
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
W3_plot_rmse_variables<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		c<-PCA3W$limits[1]
		o<-PCA3W$limits[2]
		v<-PCA3W$limits[3]
		rv<-rep(0,v)
		rv<-apply(PCA3W$dis,2,sum)
		rv<-rv/(o*c)
		barplot(rv,col='green',main='RMSE of Variables',ylim=c(0,1.2*max(rv)),names.arg=as.character(1:v),cex.names=1.0)
		box()
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
W3_plot_triplot<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		lo<-t(PCA3W$lo)
		lv<-t(PCA3W$lv)
		lc<-t(PCA3W$lc)
		ldim1<-c(min(lc[,1],lv[,1],lo[,1],0),max(lc[,1],lv[,1],lo[,1],0))
		ldim2<-c(min(lc[,2],lv[,2],lo[,2],0),max(lc[,2],lv[,2],lo[,2],0))
		plot(lo[,1],lo[,2],type='n',main='Triplot (red=objects, green=variables, blue=conditions)',
		cex.main=0.8,xlim=ldim1,ylim=ldim2,xlab='Axis 1',ylab='Axis 2')
		grid()
		text(lo[,1],lo[,2],as.character(1:PCA3W$limits[2]),col='red',cex=0.6)
		text(lv[,1],lv[,2],as.character(1:PCA3W$limits[3]),col='green',cex=0.6)
		text(lc[,1],lc[,2],as.character(1:PCA3W$limits[1]),col='blue',cex=0.6)
		points(0,0,pch='+',col='black')
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
W3_plot_variables<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		lv<-t(PCA3W$lv)
		ldim<-c(min(lv,0),max(lv,0))
		plot(lv[,1],lv[,2],type='n',main='Plot of Variables',xlim=ldim,ylim=ldim,xlab='Axis 1',ylab='Axis 2')
		grid()
		text(lv[,1],lv[,2],as.character(1:PCA3W$limits[3]),col='green',cex=0.8)
		points(0,0,pch='+',col='black')
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
W3_plot_variance<-function(){
	if(exists("PCA3W",envir=.GlobalEnv)){
		get("PCA3W",envir=.GlobalEnv)
		plot(0:(length(PCA3W$var)-1),PCA3W$var,type='l',ylab='% Variance',xlab='n. Iteration',main='Evolution of the explained variance')
		grid()
	}else{
		tk_messageBox(type=c("ok"),message='Run Model First in 3W-PCA!',caption="Input Error")
	}
}
CAL_biplot<-function(){
	if(exists("PLS",envir=.GlobalEnv)){
		get("PLS",envir=.GlobalEnv)
		ans<-inpboxe2k(c('* Component on x-axis','* Component on y-axis','Arrows'),c(1,2,'TRUE'))
		if(!is.null(ans)){
			biplot(PLS$res,comps=as.numeric(ans[[1]]):as.numeric(ans[[2]]),var.axes=as.logical(ans[[3]]))
			grid()
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',caption="Input Error")
	}
}
CAL_coefficients<-function(){
	if(exists("PLS",envir=.GlobalEnv)){
		get("PLS",envir=.GlobalEnv)
		Cm<-PLS$res$coefficients[,1,PLS$ncomp]/PLS$res$scale
		#print(Cm)
		nCm<-length(Cm)
		plot(1:nCm,Cm,xlab='Variable Number',ylab='Regression Coefficients',type='l')
		grid()
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',caption="Input Error")
	}
}
CAL_experimental_calculated<-function(){
	if(exists("PLS",envir=.GlobalEnv)){
		get("PLS",envir=.GlobalEnv)
		ans<-inpboxe2k(c('Color Vector (e.g., A[,1])','Label Vector (e.g., A[,1])','Row Names'),c('None','None','FALSE'))
		if(!is.null(ans)){
			g<-NULL
			tex<-NULL
			vcolor<-NULL
			if(ans[[1]]!='None'){
				g<-givemat(ans[[1]],nl=nrow(PLS$dataset))
				g<-factor(g)
				vcolor<-colorpanel(nlevels(g),low="red",high="green") 
			}
			if(ans[[2]]!='None'){
				tex<-givemat(ans[[2]],nl=nrow(PLS$dataset))
			}
			if(as.logical(ans[[3]]))tex<-row.names(PLS$dataset)
			ans1<-1
			if(PLS$nY>1)ans1<-inpboxc('Which Y variable :',as.character(1:PLS$nY))
			if(!is.null(ans1)){
				ms<-as.matrix(PLS$dataset[,1])[,as.numeric(ans1)]
				op<-par(pty='s',mfrow=c(1,2))
				ft<-PLS$resf$fitted.values[,as.numeric(ans1),PLS$ncomp]
				yl<-c(min(ft,ms),max(ft,ms))
				plot(ms,ft,xlab='Measured Value',ylab='Fitted Value',xlim=yl,ylim=yl,main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
				lines(par('usr')[1:2],par('usr')[3:4])
				grid()
				if((is.null(g))&(is.null(tex)))points(ms,ft,col='black')
				if((!is.null(g))&(is.null(tex)))points(ms,ft,col=vcolor[as.numeric(g)])
				if((is.null(g))&(!is.null(tex)))text(ms,ft,as.character(tex),cex=0.8)
				if((!is.null(g))&(!is.null(tex)))text(ms,ft,as.character(tex),col=vcolor[as.numeric(g)],cex=0.8)
				ft<-PLS$res$validation$pred[,as.numeric(ans1),PLS$ncomp]
				yl<-c(min(ft,ms),max(ft,ms))
				plot(ms,ft,xlab='Measured Value',ylab='CV Value',xlim=yl,ylim=yl,main=paste('Model with',PLS$ncomp,'Comp.'),type='n')
				lines(par('usr')[1:2],par('usr')[3:4])
				grid()
				if((is.null(g))&(is.null(tex)))points(ms,ft,col='black')
				if((!is.null(g))&(is.null(tex)))points(ms,ft,col=vcolor[as.numeric(g)])
				if((is.null(g))&(!is.null(tex)))text(ms,ft,as.character(tex),cex=0.8)
				if((!is.null(g))&(!is.null(tex)))text(ms,ft,as.character(tex),col=vcolor[as.numeric(g)],cex=0.8)
				par(op)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',caption="Input Error")
	}
}
CAL_extract<-function(){
	if(exists("PLS",envir=.GlobalEnv)){
		get("PLS",envir=.GlobalEnv)
		ans<-inpboxc('* Extract Matrix:',c('RMSEP','y-Loadings','y-Scores','x-Loadings','Coefficient','MSEP','Weights load.'))
		if(!is.null(ans)){
			if(ans[[1]]==1){
				rmsep<-RMSEP(PLS$res)$val[1,,]
				assign('rmsep',rmsep,envir=.GlobalEnv)
				print('Values saved in rmsep vector',quote=FALSE) 
			}
			if(ans[[1]]==2){
				yLm<-Yloadings(PLS$res)
				assign('yLm',yLm,envir=.GlobalEnv)
				print('Values saved in yLm matrix',quote=FALSE)
			}
			if(ans[[1]]==3){
				ySm<-Yscores(PLS$res)
				assign('ySm',ySm,envir=.GlobalEnv)
				print('Values saved in ySm matrix',quote=FALSE)
			}
			if(ans[[1]]==4){
				xLm<-loadings(PLS$res)
				assign('xLm',xLm,envir=.GlobalEnv)
				print('Values saved in xLm matrix',quote=FALSE)
			}
			if(ans[[1]]==5){
				Cm<-c(coef(PLS$res,intercept=TRUE)[1],coef(PLS$res,intercept=FALSE)/PLS$res$scale)
				assign('Cm',Cm,envir=.GlobalEnv)
				print('Values saved in Cm vector',quote=FALSE)
			}
			if(ans[[1]]==6){
				msep<-MSEP(PLS$res)$val[1,,]
				assign('msep',msep,envir=.GlobalEnv)
				print('Values saved in msep vector',quote=FALSE)
			}
			if(ans[[1]]==7){
				wLm<-loading.weights(PLS$res)
				assign('wLm',wLm,envir=.GlobalEnv)
				print('Values saved in wLm matrix',quote=FALSE)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',caption="Input Error")
	}
}
CAL_model_computation_PCR<-function(previous.name=''){
    if(exists("PLS",envir=.GlobalEnv))rm("PLS",envir=.GlobalEnv)
	if(!exists('pls.set',envir=.GlobalEnv))pls.set<-c(previous.name,'all','','','10','5','TRUE','TRUE')
	ans<-inpboxe6k2(c('*Matrix Name','*Rows to be selected (e.g., 1:10,15)',
	'*X-Variables to be selected (e.g., 1:4,8)','*Y-Variable to be selected (e.g., 9)',
	'*Number of Components','*Number of Segments for CV','Centered','Scaled'),pls.set)
	if(!is.null(ans)){
		PLS<-list()
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if(ans[[4]]!='None'){
			if(is.na(as.numeric(ans[[4]]))){
				Y<-givemat(ans[[4]],nl=nrow(M))
			}else{
				Y<-M[,as.numeric(ans[[4]])] 
				if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
			}
		}else{
			Y<-rep(0,nrow(M))
			if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
		}
		if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
		if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
		if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
		if((typeof(M)=='double')|(typeof(M)=='list')){
			M<-data.frame(cbind(Y,data.frame(M)))
			naM<-names(M)
			nNA<-sum(is.na(M))
			nY<-1
			if(nNA>0){
				mess<-paste(as.character(nNA),'NA present.We try to rebuild them!')
				tk_messageBox(type=c("ok"),message=mess,caption="Input Error")
				md<-prep(M,scale="uv",center=TRUE,simple=FALSE,reverse=FALSE)
				res<-pca(md$data,method="nipals",nPcs=min(ncol(M),10),scale="uv",center=TRUE)
				M<-prep(res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
				M<-as.data.frame(M)
			}
			ncompo<-min(as.numeric(ans[[5]]),ncol(M)-1)
			model<-paste(naM[nY],'~',(paste(naM[-nY],collapse='+')),sep='')
			res<-pcr(as.formula(model),ncomp=ncompo,data=M,segment.type="interleaved",
			validation='CV',segments=as.numeric(ans[[6]]),scale=as.logical(ans[[8]]))
			resf<-pcr(as.formula(model),ncomp=ncompo,data=M,validation='none',
			scale=as.logical(ans[[8]]))
			rmsep<-RMSEP(res,intercep=FALSE)
			op<-par(pty='s',mfrow=c(1,2))
			plot(rmsep,xlab='Number of Components',ylab='RMSECV',main='')
			grid()
			mtext('Black:RMSECV; Red:adjRMSECV',side=3,line=0,cex=0.6)
			vm<-R2(res,estimate='CV',ncomp=1:ncompo,intercept=FALSE)$val[1,,]*100
			plot(vm,xlab='Number of Components',ylab='CV % Explained Variance',ylim=c(0,100))
			grid()
			par(op)
			print('',quote=FALSE)
			print('CV% Explained Variance',quote=FALSE)
			print(vm,quote=FALSE)
			print('',quote=FALSE)
			print('RMSECV',quote=FALSE)
			print(rmsep,quote=FALSE)
			print('',quote=FALSE)
			print(paste('Minimum value found at component n.:',which.min(rmsep$val[1,,])),quote=FALSE)
			pls.set<-ans
			PLS$typ<-'PCR'
			PLS$dataset<-M
			PLS$nY<-nY
			PLS$validation<-'CV'
			PLS$nseg<-as.integer(ans[[6]])
			PLS$segtype<-'interleaved'
			PLS$center<-as.logical(ans[[7]])
			PLS$scale<-as.logical(ans[[8]])
			PLS$model<-as.formula(model)
			ans<-inpboxc('Number of Components:',as.character(1:ncompo))
			if(!is.null(ans)){
				PLS$ncomp<-as.numeric(ans)
				pls.set[5]<-PLS$ncomp
				res<-plsr(PLS$model,ncomp=PLS$ncomp,data=PLS$dataset,segment.type="interleaved",
				validation='CV',segments=PLS$nseg,scale=PLS$scale)
				PLS$rmsep<-RMSEP(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]
				PLS$rcv<-R2(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]*100
				print(paste('Model created with ',format(PLS$ncomp,digits=2),' components and saved in PLS object',sep=''),quote=FALSE)
				PLS$res<-res
				PLS$resf<-resf
			}
			assign('PLS',PLS,envir=.GlobalEnv)
		    assign('pls.set',pls.set,envir=.GlobalEnv)
		}else{
			tk_messageBox(type=c("ok"),message='Matrix/Table Requested !',caption="Input Error")
		}
	}
}
CAL_model_computation_PLS1<-function(previous.name=''){
    if(exists("PLS",envir=.GlobalEnv))rm("PLS",envir=.GlobalEnv)
	if(!exists('pls.set',envir=.GlobalEnv))pls.set<-c(previous.name,'all','','','10','5','TRUE','TRUE')
	ans<-inpboxe6k2(c('*Matrix Name','*Rows to be selected (e.g., 1:10,15)',
	'*X-Variables to be selected (e.g., 1:4,8)','*Y-Variable to be selected (e.g., 9)',
	'*Number of Components','*Number of Segments for CV','Centered','Scaled'),pls.set)
	if(!is.null(ans)){
		PLS<-list()
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if(ans[[4]]!='None'){
			if(is.na(as.numeric(ans[[4]]))){
				Y<-givemat(ans[[4]],nl=nrow(M))
			}else{
				Y<-M[,as.numeric(ans[[4]])] 
			if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
			}
		}else{
			Y<-rep(0,nrow(M))
			if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
		}
		if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
		if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
		if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
		if((typeof(M)=='double')|(typeof(M)=='list')){
			M<-data.frame(cbind(Y,data.frame(M)))
			naM<-names(M)
			nNA<-sum(is.na(M))
			nY<-1
			if(nNA>0){
				mess<-paste(as.character(nNA),'NA present.We try to rebuild them!')
				tk_messageBox(type=c("ok"),message=mess,caption="Input Error")
				md<-prep(M,scale="uv",center=TRUE,simple=FALSE,reverse=FALSE)
				res<-pca(md$data,method="nipals",nPcs=min(ncol(M),10),scale="uv",center=TRUE)
				M<-prep(res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
				M<-as.data.frame(M)
			}
			ncompo<-min(as.numeric(ans[[5]]),ncol(M)-1)
			model<-paste(naM[nY],'~',(paste(naM[-nY],collapse='+')),sep='')
			res<-plsr(as.formula(model),ncomp=ncompo,data=M,segment.type="interleaved",validation='CV',segments=as.numeric(ans[[6]]),scale=as.logical(ans[[8]]))
			resf<-plsr(as.formula(model),ncomp=ncompo,data=M,validation='none',scale=as.logical(ans[[8]]))
			rmsep<-RMSEP(res,intercep=FALSE)
			op<-par(pty='s',mfrow=c(1,2))
			plot(rmsep,xlab='Number of Components',ylab='RMSECV',main='')
			grid()
			mtext('Black:RMSECV; Red:adjRMSECV',side=3,line=0,cex=0.6)
			vm<-R2(res,estimate='CV',ncomp=1:ncompo,intercept=FALSE)$val[1,,]*100
			plot(vm,xlab='Number of Components',ylab='CV % Explained Variance',ylim=c(0,100))
			grid()
			par(op)
			print('',quote=FALSE)
			print('CV% Explained Variance',quote=FALSE)
			print(vm,quote=FALSE)
			print('',quote=FALSE)
			print('RMSECV',quote=FALSE)
			print(rmsep,quote=FALSE)
			print('',quote=FALSE)
			print(paste('Minimum value found at component n.:',which.min(rmsep$val[1,,])),quote=FALSE)
			pls.set<-ans
			PLS$typ<-'PLS1'
			PLS$dataset<-M
			PLS$nY<-nY
			PLS$validation<-'CV'
			PLS$nseg<-as.numeric(ans[[6]])
			PLS$segtype<-'interleaved'
			PLS$center<-as.logical(ans[[7]])
			PLS$scale<-as.logical(ans[[8]])
			PLS$model<-as.formula(model)
			ans<-inpboxc('Number of Components:',as.character(1:ncompo),which.min(rmsep$val[1,,])-1)
			if(!is.null(ans)){
				PLS$ncomp<-as.numeric(ans)
				pls.set[5]<-PLS$ncomp
				res<-plsr(PLS$model,ncomp=PLS$ncomp,data=PLS$dataset,segment.type="interleaved",validation='CV',segments=PLS$nseg,scale=PLS$scale)
				PLS$rmsep<-RMSEP(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]
				PLS$rcv<-R2(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]*100
				print(paste('Model created with ',format(PLS$ncomp,digits=2),' components and saved in PLS object',sep=''),quote=FALSE)
				PLS$res<-res
				PLS$resf<-resf
			}
			assign('PLS',PLS,envir=.GlobalEnv)
		    assign('pls.set',pls.set,envir=.GlobalEnv)
		}else{
			tk_messageBox(type=c("ok"),message='Matrix/Table Requested !',caption="Input Error")
		}
	}
}
CAL_model_computation_PLS2<-function(previous.name=''){
    if(exists("PLS",envir=.GlobalEnv))rm("PLS",envir=.GlobalEnv)
	if(!exists('pls.set',envir=.GlobalEnv))pls.set<-c(previous.name,'all','','','10','5','TRUE','TRUE')
	ans<-inpboxe6k2(c('*Matrix Name','*Rows to be selected (e.g., 1:10,15)',
	'*X-Variables to be selected (e.g., 1:4,8)','*Y-Variables to be selected (e.g., 8:9,11)',
	'*Number of Components','*Number of Segments for CV','Centered','Scaled'),pls.set)
	if(!is.null(ans)){
		PLS<-list()
 		name<-ans[[1]]
		X<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		pls.set<-ans
		Y<-X
		if((ans[[2]]!='all')&(ans[[3]]!='all'))X<-X[givedim(ans[[2]]),givedim(ans[[3]])]
		if((ans[[2]]!='all')&(ans[[3]]=='all'))X<-X[givedim(ans[[2]]),]
		if((ans[[2]]=='all')&(ans[[3]]!='all'))X<-X[,givedim(ans[[3]])]
		if((ans[[2]]!='all')&(ans[[4]]!='all'))Y<-Y[givedim(ans[[2]]),givedim(ans[[4]])]
		if((ans[[2]]!='all')&(ans[[4]]=='all'))Y<-Y[givedim(ans[[2]]),]
		if((ans[[2]]=='all')&(ans[[4]]!='all'))Y<-Y[,givedim(ans[[4]])]
		M<-data.frame(Y=I(as.matrix(Y)),data.frame(X))
		nY<-ncol(Y)
		if((typeof(M)=='double')|(typeof(M)=='list')){
			naM<-names(M)
			nNA<-sum(is.na(M))
			if(nNA>0){
				mess<-paste(as.character(nNA),'NA present.We try to rebuild them!')
				tk_messageBox(type=c("ok"),message=mess,caption="Input Error")
				md<-prep(M,scale="uv",center=TRUE,simple=FALSE,reverse=FALSE)
				res<-pca(md$data,method="nipals",nPcs=min(ncol(M),10),scale="uv",center=TRUE)
				M<-prep(res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
				M<-as.data.frame(M)
			}
			ncompo<-min(as.numeric(ans[[5]]),ncol(M)-nY)
			model<-paste('Y ~',(paste(naM[-1],collapse='+')),sep='')
			res<-plsr(as.formula(model),ncomp=ncompo,data=M,segment.type="interleaved",validation='CV',segments=as.numeric(ans[[6]]),scale=as.logical(ans[[8]]))
			resf<-plsr(as.formula(model),ncomp=ncompo,data=M,validation='none',scale=as.logical(ans[[8]]))
			rmsep<-RMSEP(res,intercep=FALSE)
			vm<-R2(res,estimate='CV',ncomp=1:ncompo,intercept=FALSE)$val[1,,]*100
			print(rmsep,quote=FALSE)
			print(vm,quote=FALSE)
			pls.set<-ans
			PLS$typ<-'PLS2'
			PLS$dataset<-M
			PLS$nY<-nY
			PLS$nX<-(ncol(Y)+1):(ncol(Y)+ncol(X))
			PLS$validation<-'CV'
			PLS$nseg<-as.numeric(ans[[6]])
			PLS$segtype<-'interleaved'
			PLS$center<-as.logical(ans[[7]])
			PLS$scale<-as.logical(ans[[8]])
			PLS$model<-as.formula(model)
			ans<-inpboxc('Number of Components:',as.character(1:ncompo))
			if(!is.null(ans)){
				PLS$ncomp<-as.numeric(ans)
				pls.set[5]<-PLS$ncomp
				res<-plsr(PLS$model,ncomp=PLS$ncomp,data=PLS$dataset,segment.type="interleaved",validation='CV',segments=PLS$nseg,scale=PLS$scale)
				PLS$rmsep<-RMSEP(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]
				PLS$rcv<-R2(res,estimate='CV',ncomp=PLS$ncomp,intercept=FALSE)$val[1,,]*100
				print(paste('Model created with ',format(PLS$ncomp,digits=2),' components and saved in PLS object',sep=''),quote=FALSE)
				PLS$res<-res
				PLS$resf<-resf
			}
			assign('PLS',PLS,envir=.GlobalEnv)
		    assign('pls.set',pls.set,envir=.GlobalEnv)
		}else{
			tk_messageBox(type=c("ok"),message='Matrix/Table Requested !',caption="Input Error")
		}
	}
}
CAL_prediction<-function(previous.name=''){
	if(exists("PLS",envir=.GlobalEnv)){
		get("PLS",envir=.GlobalEnv)	
		if(PLS$typ!='PLS2'){
			ans<-inpboxe4k2(c('*Matrix Name with samples to be predicted','*Rows to be selected (e.g., 1:10,15)','*X-Variables to be selected (e.g., 1:4,8)',
			'*Y-Variable to be selected (e.g., 9)','Row Names',''),c(previous.name,'all','all','None','FALSE','FALSE'))
			if(!is.null(ans)){
				name<-ans[[1]]
				assign('previous.name',name,envir=.GlobalEnv)
				M<-givemat(name)
				M<-data.frame(M)
				loY<-TRUE
				if(ans[[4]]!='None'){
					if(is.na(as.numeric(ans[[4]]))){
						Y<-givemat(ans[[4]],nl=nrow(M))
					}else{
						Y<-M[,as.numeric(ans[[4]])] 
						if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
					}
				}else{
					loY<-FALSE
					Y<-rep(0,nrow(M))
					if(ans[[2]]!='all')Y<-Y[givedim(ans[[2]])]
				}
				if((ans[[2]]!='all')&(ans[[3]]!='all'))M<-M[givedim(ans[[2]]),givedim(ans[[3]])]
				if((ans[[2]]!='all')&(ans[[3]]=='all'))M<-M[givedim(ans[[2]]),]
				if((ans[[2]]=='all')&(ans[[3]]!='all'))M<-M[,givedim(ans[[3]])]
				prm<-drop(predict(PLS$res,newdata=M,ncomp=1:PLS$ncomp,scale=PLS$scale))
				prm.tr<-drop(predict(PLS$res,newdata=NULL,ncomp=1:PLS$ncomp,scale=PLS$scale))
				rmsep<-RMSEP(PLS$res,estimate='test',newdata=M,ncomp=PLS$ncomp,scale=PLS$scale,intercept=FALSE)$val[1,,]
				if(nrow(M)==1)prm<-matrix(unlist(prm),1,PLS$ncomp)
				res<-prm[,PLS$ncomp]-Y
				res.tr<-prm.tr[,PLS$ncomp]-PLS$dataset[,1]
				if(loY){
					print('Prediction Statistics',quote=FALSE)
					print('',quote=FALSE)
					print(paste('RMSEP:',format(rmsep,digits=4)),quote=FALSE)
					print(paste('BIAS :',format(mean(res),digits=4)),quote=FALSE)
					print('',quote=FALSE)
					op<-par(pty='s',mfrow=c(1,2))
					if(!as.logical(ans[[5]])){
						plot(Y,prm[,PLS$ncomp],xlab='Experimental Value',ylab='Predicted Value',asp=1,
						xlim=c(min(c(Y,prm[,PLS$ncomp],prm.tr[,PLS$ncomp])),max(c(Y,prm[,PLS$ncomp],prm.tr[,PLS$ncomp]))),
						ylim=c(min(c(Y,prm[,PLS$ncomp],prm.tr[,PLS$ncomp])),max(c(Y,prm[,PLS$ncomp],prm.tr[,PLS$ncomp]))))
						lines(par('usr')[1:2],par('usr')[3:4],col='red')
						grid()
						plot(1:nrow(M),res,xlab='Object Number',ylab='Residuals',
						ylim=c(min(min(res),min(res.tr)),max(c(res,res.tr))))
						abline(h=0,col="red")
						grid()
					}else{
						plot(Y,prm[,PLS$ncomp],xlab='Experimental Value',ylab='Predicted Value',asp=1,
						xlim=c(min(c(Y,prm[,PLS$ncomp],prm.tr[,PLS$ncomp])),max(c(Y,prm[,PLS$ncomp],prm.tr[,PLS$ncomp]))),
						ylim=c(min(c(Y,prm[,PLS$ncomp],prm.tr[,PLS$ncomp])),max(c(Y,prm[,PLS$ncomp],prm.tr[,PLS$ncomp]))),type='n')
						text(Y,prm[,PLS$ncomp],as.character(row.names(M)))
						lines(par('usr')[1:2],par('usr')[3:4],col='red')
						grid()
						plot(1:nrow(M),res,xlab='Object Number',ylab='Residuals',type='n',
						ylim=c(min(c(res,res.tr)),max(c(res,res.tr))))
						text(1:nrow(M),res,as.character(row.names(M)))
						abline(h=0,col="red")
						grid()
					}
					par(op)
				}
				print('Predicted Values',quote=FALSE)
				print(prm[,PLS$ncomp])
				assign('var.fitted',prm[,PLS$ncomp],envir=.GlobalEnv)
				print('The value is saved in: var.fitted',quote=FALSE)
			}
		}else{
			tk_messageBox(type=c("ok"),message='Not implemented for PLS2',caption="Input Error")
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',caption="Input Error")
	}
}
CAL_residuals<-function(){
	if(exists("PLS",envir=.GlobalEnv)){
		get("PLS",envir=.GlobalEnv)
		ans<-inpboxe2k(c('Color Vector (e.g., A[,1])','Label Vector (e.g., A[,1])','Row Names'),c('None','None','FALSE'))
		if(!is.null(ans)){
			g<-NULL
			tex<-NULL
			vcolor<-NULL
			if(ans[[1]]!='None'){
				g<-givemat(ans[[1]],nl=nrow(PLS$dataset))
				g<-factor(g)
				vcolor<-colorpanel(nlevels(g),low="red",high="green") 
			}
			if(ans[[2]]!='None'){
				tex<-givemat(ans[[2]],nl=nrow(PLS$dataset))
			}
			if(as.logical(ans[[3]]))tex<-row.names(PLS$dataset)
			ans1<-1
			if(PLS$nY>1)ans1<-inpboxc('Which Y variable :',as.character(1:PLS$nY))
			if(!is.null(ans1)){
				ms<-as.matrix(PLS$dataset[,1])[,as.numeric(ans1)]
				op<-par(pty='s',mfrow=c(1,2))
				rs<-PLS$resf$fitted.values[,as.numeric(ans1),PLS$ncomp]-ms
				plot(1:length(rs),rs,type='n',xlab='Object Number',ylim=c(min(0,rs),max(0,rs)),ylab=paste('Residuals in Fitting with ',PLS$ncomp,' Comp.'))
				grid()
				abline(h=0,col="red")
				if((is.null(g))&(is.null(tex)))points(1:length(rs),rs,col='black')
				if((!is.null(g))&(is.null(tex)))points(1:length(rs),rs,col=vcolor[as.numeric(g)])
				if((is.null(g))&(!is.null(tex)))text(1:length(rs),rs,as.character(tex),cex=0.8)
				if((!is.null(g))&(!is.null(tex)))text(1:length(rs),rs,as.character(tex),col=vcolor[as.numeric(g)],cex=0.8)
				rs<-PLS$res$validation$pred[,as.numeric(ans1),PLS$ncomp]-ms
				plot(1:length(rs),rs,xlab='Object Number',ylab=paste('Residuals in CV with ',PLS$ncomp,' Comp.'),type='n',ylim=c(min(0,rs),max(0,rs)))
				grid()
				abline(h=0,col="red")
				if((is.null(g))&(is.null(tex)))points(1:length(rs),rs,col='black')
				if((!is.null(g))&(is.null(tex)))points(1:length(rs),rs,col=vcolor[as.numeric(g)])
				if((is.null(g))&(!is.null(tex)))text(1:length(rs),rs,as.character(tex),cex=0.8)
				if((!is.null(g))&(!is.null(tex)))text(1:length(rs),rs,as.character(tex),col=vcolor[as.numeric(g)],cex=0.8)
				par(op)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',caption="Input Error")
	}
}
CAL_scores<-function(){
	if(exists("PLS",envir=.GlobalEnv)){
		get("PLS",envir=.GlobalEnv)
		ans<-inpboxe4r2(c('*Component on x-axis', '*Component on y-axis','Label Vector (e.g., A[,1])','Color Vector (e.g., A[,1])','Same scale','Different scales'),
		c('1','2','None','None','TRUE','FALSE'))
		if(!is.null(ans)){
			n1<-as.integer(ans[[1]])
			n2<-as.integer(ans[[2]])
			if((n1<=PLS$ncomp)&(n2<=PLS$ncomp)){
				Ms<-PLS$res$scores
				if(as.logical(ans[[5]])){
					yl<-c(min(Ms[,n1],Ms[,n2]),max(Ms[,n1],Ms[,n2]))
					xl<-yl
				}else{ 
					yl<-c(min(Ms[,n2]),max(Ms[,n2]))
					xl<-c(min(Ms[,n1]),max(Ms[,n1]))
				}
				tex<-NULL
				if(!is.null(rownames(Ms)))tex<-rownames(Ms)
				if(as.character(ans[[3]])!='None')tex<-givemat(ans[[3]],nl=nrow(Ms))
				grade<-NULL
				if(as.character(ans[[4]])!='None'){
					grade<-givemat(ans[[4]],nl=nrow(Ms))
					grade<-factor(grade)
					lev<-levels(grade)
					nl<-nlevels(grade)
					vcolor<-colorpanel(nl,low="red",high="green")
				}
				if(is.null(tex) & is.null(grade)){
					plot(Ms[,n1],Ms[,n2],xlab=paste('Comp.',n1,sep=''),ylab=paste('Comp.',n2,sep=''),xlim=xl,ylim=yl,pty='o',col='black')
					grid()
				}
				if(!is.null(tex)& is.null(grade)){
					plot(Ms[,n1],Ms[,n2],type='n',xlab=paste('Comp.',n1,sep=''),ylab=paste('Comp.',n2,sep=''),xlim=xl,ylim=yl)
					grid()
					text(Ms[,n1],Ms[,n2],as.character(tex),col='black',cex=0.8)
				}
				if(is.null(tex)&!is.null(grade)){
					plot(Ms[,n1],Ms[,n2],type='n',xlab=paste('Comp.',n1,sep=''),ylab=paste('Comp.',n2,sep=''),xlim=xl,ylim=yl)
					grid()
					for(i in 1:nl){
						points(subset(Ms[,c(n1,n2)],grade==lev[i]),pch=19,col=vcolor[i])
					}
				}
				if(!is.null(tex)& !is.null(grade)){
					plot(Ms[,n1],Ms[,n2],type='n',xlab=paste('Comp.',n1,sep=''),ylab=paste('Comp.',n2,sep=''),xlim=xl,ylim=yl)
					grid()
					for(i in 1:nl){
						text(subset(Ms[,c(n1,n2)],grade==lev[i]),as.character(subset(tex,grade==lev[i])),col=vcolor[i],cex=0.8)
					}
				}
				text(0,0,'+',cex=1.2,col='red')
			}else{
				tk_messageBox(type=c("ok"),message='Number component too high!',caption="Input Error")
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',caption="Input Error")
	}
}
CAL_xloadings<-function(){
	if(exists("PLS",envir=.GlobalEnv)){
		get("PLS",envir=.GlobalEnv)
		ans<-inpboxr2(c('Graph Type  ','Scatter','Lines'))
		if(!is.null(ans)){
			if(as.logical(ans[[1]])){
				ans1<-inpboxe3k2(c('*Component on x-axis','*Component on y-axis','Label Vector (e.g., A[,1])','Column Names','Arrows'),
				c('1','2','None','FALSE','FALSE'))
				if(!is.null(ans1)){
					n1<-as.integer(ans1[[1]])
					n2<-as.integer(ans1[[2]])
					T<-loadings(PLS$res)
					tex<-as.character(1:nrow(T))
					if(ans[[3]]!='None'){
						tex<-givemat(ans[[3]],nrow(T))
					}
					if(as.logical(ans[[4]]))tex<-row.names(T)
					Tlim<-c(min(T[,c(n1,n2)]),max(T[,c(n1,n2)]))
					Tlim<-c(sign(Tlim[1])*max(abs(Tlim)),sign(Tlim[2])*max(abs(Tlim)))
					plot(T[,n1],T[,n2],xlab=paste('Comp.',n1),ylab=paste('Comp.',n2),main='X-loading Plot',type='n',xlim=Tlim,ylim=Tlim)
					text(T[,n1],T[,n2],tex,cex=0.6)
					text(0,0,'+',cex=1.2,col='red')
					grid()
					if(as.logical(ans[[5]]))arrows(rep(0,dim(T)[1]),rep(0,dim(T)[2]),T[,n1],T[,n2],col='red')
				}
			}else{
				ans1<-inpboxe1('*Components to be plotted (e.g.,1,3,5)','1,2')
				T<-loadings(PLS$res)
				plot(T[,1],ylab='x-loading value',xlab='Variable',type='n',ylim=c(min(T),max(T)))
				vi<-as.numeric(unlist(str_split(ans1[[1]],',')))
				grid()
				for(i in vi)lines(T[,i],col=i)
				legend("bottomleft",legend=as.character(vi),col=vi,lty=1)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message='Run Matrix Evaluation First in PCR/PLS1/PLS2!',caption="Input Error")
	}
}
CL_extract<-function(){
	if(exists("CLA",envir=.GlobalEnv)){
		get("CLA",envir=.GlobalEnv)
		ans<-inpboxc('*Extract Matrix:',c('Means','Covariance','Md-CV','Prediction'))
			if(!is.null(ans)){
				if(ans[[1]]==1){
					Mea<-CLA$means
					assign('Mea',Mea,envir=.GlobalEnv)
					print('Values saved in Mea vector',quote=FALSE)
				}
				if(ans[[1]]==2){
					Cov<-CLA$cov
					assign('Cov',Cov,envir=.GlobalEnv)
					print('Values saved in Cov vector',quote=FALSE)
				}
				if(ans[[1]]==3){
					MCV<-CLA$MdCV
					assign('MCV',MCV,envir=.GlobalEnv)
					print('Values saved in MCV matrix',quote=FALSE)
				}
				if((ans[[1]]==4)&(!is.null(CLA$pred))){
					Pre<-CLA$pred
					assign('Pre',Pre,envir=.GlobalEnv)
					print('Values saved in Pre matrix',quote=FALSE)
				}
			}
		}else{
			tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
		}
}
CL_method_LDA<-function(previous.name=''){
	ans<-inpboxe5(c('*Matrix Name','*Rows to be selected (e.g., 1:10,15)','*X-Variables to be selected (e.g., 1:4,8)',
	'*Category Variable','*Number of Segments for CV'),c(previous.name,'all','','1','5'))
	if(!is.null(ans)){
		CLA<-list()
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if(ans[[2]]!='all')M<-M[givedim(ans[[2]]),]
		gr<-factor(M[,as.integer(ans[[4]])])
		M<-M[,givedim(ans[[3]])]
		CLA$dataset<-M
		M<-as.data.frame(M)
		vv<-ncol(M)
		rr<-nrow(M)
		ng<-as.numeric(ans[[5]])
		lev<-levels(factor(gr))
		cat<-nlevels(gr)
		numcat<-summary(gr)
		co<-matrix(rep(0,vv^2),vv,vv)
		me<-matrix(rep(0,vv*cat),cat,vv)
		d2<-matrix(rep(0,rr*cat),rr,cat)
		for (i in 1:cat){
			me[i,]<-apply(M[gr==lev[i],],2,mean)
			co<-co+cov(M[gr==lev[i],])*(sum(gr==lev[i])-1)/(rr-cat)
		}
		for(i in 1:rr){
			for(j in 1:cat){
				d2[i,j]<-as.matrix(M[i,]-me[j,])%*%matrix.inverse(co)%*%t(as.matrix(M[i,]-me[j,]))/(rr-1)*rr
			}
		}
		CLA$means<-me
		CLA$cov<-co
		CLA$dataset<-M
		CLA$group<-gr
		CLA$lev<-lev
		CLA$lb.obj<-unlist(dimnames(CLA$dataset)[1])
		if(is.null(CLA$lb.obj))CLA$lb.obj<-as.character(1:rr)
		#cross validation calculation
		d2<-matrix(rep(0,rr*cat),rr,cat)
		for (g in 1:ng){
			t.seq<-seq(g,rr,by=ng)
			Mtr<-M[-t.seq,]
			grt<-gr[-t.seq]
			Mev<-M[t.seq,]
			co<-matrix(rep(0,vv^2),vv,vv)
			me<-matrix(rep(0,vv*cat),cat,vv)
			for (i in 1:cat){
				me[i,]<-apply(Mtr[grt==lev[i],],2,mean)
				co<-co+cov(Mtr[grt==lev[i],])*(sum(grt==lev[i])-1)/(nrow(Mtr)-cat)
			}
			for (i in 1:length(t.seq)){
				for (j in 1:cat){
					d2[t.seq[i],j]<-as.matrix(Mev[i,]-me[j,])%*%matrix.inverse(co)%*%t(as.matrix(Mev[i,]-me[j,]))
				}
			}
		}
		dd<-t(d2)
		CLA$MdCV<-dd
		print('Confusion Matrix in Cross Validation',quote=FALSE)
		mt<-table(gr,apply(dd,2,which.min))
		dimnames(mt)<-list(lev,lev)
		CLA$mt<-mt
		print(mt,quote=FALSE)
		diag(mt)<-0
		errcat<-apply(mt,1,sum)
		if(sum(errcat)!=0){
			print('Labels of Samples with wrong assignment',quote=FALSE)
			print(row.names(M)[which(lev[apply(dd,2,which.min)]!=gr)],quote=FALSE)
		}
		print('% Correct Predictions in Cross Validation',quote=FALSE)
		print(format(100*(1-(errcat/numcat)),digits=4),quote=FALSE)
		print('% Total Correct Predictions in Cross Validation',quote=FALSE)
		print(format(100*(1-mean(errcat/numcat)),digits=4),quote=FALSE)
		print('Note: Data are saved in the CLA object, write CLA to see all',quote=FALSE)
	}
}
CL_method_QDA<-function(previous.name=''){
	ans<-inpboxe5(c('*Matrix Name','*Rows to be selected (e.g., 1:10,15)','*X-Variables to be selected (e.g., 1:4,8)',
	'*Category Variable','*Number of Segments for CV'),c(previous.name,'all','','1','5'))
	if(!is.null(ans)){
		CLA<-list()
		name<-ans[[1]]
		M<-givemat(name)
		assign('previous.name',name,envir=.GlobalEnv)
		if(ans[[2]]!='all')M<-M[givedim(ans[[2]]),]
		gr<-factor(M[,as.integer(ans[[4]])])
		M<-M[,givedim(ans[[3]])]
		CLA$dataset<-M
		M<-as.data.frame(M)
		vv<-ncol(M)
		rr<-nrow(M)
		ng<-as.numeric(ans[[5]])
		lev<-levels(factor(gr))
		cat<-nlevels(gr)
		numcat<-summary(gr)
		co<-matrix(rep(0,vv^2),vv,vv)
		me<-matrix(rep(0,vv*cat),cat,vv)
		d2<-matrix(rep(0,rr*cat),rr,cat)
		for (j in 1:cat){
			me[j,]<-apply(M[gr==lev[j],],2,mean)
			co<-cov(M[gr==lev[j],])
			for(i in 1:rr){
				d2[i,j]<-as.matrix(M[i,]-me[j,])%*% matrix.inverse(co)%*%t(as.matrix(M[i,]-me[j,]))
				if(gr[i]==lev[j]){
					d2[i,j]<-d2[i,j]/sum(gr==lev[j])*(sum(gr==lev[j])+1)
				}
			}
		}
		CLA$means<-me
		CLA$cov<-co
		CLA$dataset<-M
		CLA$group<-gr
		CLA$lev<-lev
		CLA$lb.obj<-unlist(dimnames(CLA$dataset)[1])
		if(is.null(CLA$lb.obj))CLA$lb.obj<-as.character(1:rr)
		#cross validation calculation
		d2<-matrix(rep(0,rr*cat),rr,cat)
		me<-matrix(rep(0,vv*cat),cat,vv)
		for(j in (1:cat)){
			for (g in 1:ng){
				t.seq<-seq(g,rr,by=ng)
				Mtr<-M[-t.seq,]
				grt<-gr[-t.seq]
				Mev<-M[t.seq,]
				co<-matrix(rep(0,vv^2),vv,vv)
				me[j,]<-apply(Mtr[grt==lev[j],],2,mean)
				co<-cov(Mtr[grt==lev[j],])
				for (i in 1:length(t.seq)){
					d2[t.seq[i],j]<-as.matrix(Mev[i,]-me[j,])%*% matrix.inverse(co)%*%t(as.matrix(Mev[i,]-me[j,]))
				}
			}
		}
		dd<-t(d2)
		CLA$MdCV<-dd
		print('Confusion Matrix in Cross Validation',quote=FALSE)
		mt<-table(gr,apply(dd,2,which.min))
		dimnames(mt)<-list(lev,lev)
		CLA$mt<-mt
		print(mt,quote=FALSE)
		diag(mt)<-0
		errcat<-apply(mt,1,sum)
		if(sum(errcat)!=0){
			print('Labels of Samples with wrong assignment',quote=FALSE)
			print(row.names(M)[which(lev[apply(dd,2,which.min)]!=gr)],quote=FALSE)
		}
		print('% Correct Predictions in Cross Validation',quote=FALSE)
		print(format(100*(1-(errcat/numcat)),digits=4),quote=FALSE)
		print('% Total Correct Predictions in Cross Validation',quote=FALSE)
		print(format(100*(1-mean(errcat/numcat)),digits=4),quote=FALSE)
		print('Note: Data are saved in the CLA object, write CLA to see all',quote=FALSE)
	}
}
CL_plot_mahalanobis<-function(){
	if(exists("CLA",envir=.GlobalEnv)){
		barplot(apply(CLA$MdCV,2,min),main='Mahalanobis dist. from the closest category (CV)',names.arg=CLA$lb.obj,cex.names=0.5,las=2)
		grid()
		box()
	}else{
		tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
	}
}
CL_plot_mahalanobis_cat<-function(){
	if(exists("CLA",envir=.GlobalEnv)){
		get("CLA",envir=.GlobalEnv)
		ans<-inpboxc('Category',CLA$lev,inp=-1)
		if(!is.null(ans)){
			barplot(CLA$MdCV[as.numeric(ans[[1]]),],main='Mahalanobis distance (Cross Validation)',names.arg=CLA$lb.obj,cex.names=0.5,las=2)
			grid()
			box()
		}	
	}else{
		tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
	}
}
CL_plot_mahalanobis_obj<-function(){
	if(exists("CLA",envir=.GlobalEnv)){
		get("CLA",envir=.GlobalEnv)
		ans<-inpboxc('Object',CLA$lb.obj,inp=-1)
		if(!is.null(ans)){
			barplot(CLA$MdCV[,as.numeric(ans[[1]])],main='Mahalanobis distance (Cross Validation)',names.arg=CLA$lev,cex.names=1)
			grid()
			box()
		}
	}else{
		tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
	}
}
CL_pre_mahalanobis<-function(){
	if(exists("CLA",envir=.GlobalEnv)){
		get("CLA",envir=.GlobalEnv)
		if(!is.null(CLA$Md.pre)){
			barplot(apply(CLA$Md.pre,2,min),main='Mahalanobis dist. from the closest category',names.arg=CLA$lb.obj.pre,cex.names=0.5,las=2)
			grid()
			box()
		}else{
			tk_messageBox(type=c("ok"),message="Run Prediction first!",caption="Input Error")
		}
	}else{
		tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
	}
}
CL_pre_mahalanobis_cat<-function(){
	if(exists("CLA",envir=.GlobalEnv)){
		get("CLA",envir=.GlobalEnv)
		if(!is.null(CLA$Md.pre)){
			ans<-inpboxc('Category',CLA$lev,inp=-1)
			if(!is.null(ans)){
				barplot(CLA$Md.pre[as.numeric(ans[[1]]),],main='Mahalanobis distance',names.arg=CLA$lb.obj.pre,cex.names=0.5,las=2)
				grid()
				box()
			}
		}else{
			tk_messageBox(type=c("ok"),message="Run Prediction first!",caption="Input Error")
		}	
	}else{
		tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
	}
}
CL_pre_mahalanobis_obj<-function(){
	if(exists("CLA",envir=.GlobalEnv)){
		get("CLA",envir=.GlobalEnv)
		if(!is.null(CLA$Md.pre)){
			ans<-inpboxc('Object',CLA$lb.obj.pre,inp=-1)
			if(!is.null(ans)){
				barplot(CLA$Md.pre[,as.numeric(ans[[1]])],main='Mahalanobis distance',names.arg=CLA$lev,cex.names=1)
				grid()
				box()
			}
		}else{
			tk_messageBox(type=c("ok"),message="Run Prediction first!",caption="Input Error")
		}	
	}else{
		tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
	}
}
CL_prediction_LDA<-function(previous.name=''){
	if(exists("CLA",envir=.GlobalEnv)){
		get("CLA",envir=.GlobalEnv)
		ans<-inpboxe4(c('*Matrix Name with samples to be predicted',
				'*Rows to be selected (e.g., 1:10,15)',
				'*X-Variables to be selected (e.g., 1:4,8)',
				'Category Variable'),c(previous.name,'all','','None'))
		if(!is.null(ans)){
			name<-ans[[1]]
			Mtest<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			Mtest<-as.data.frame(Mtest)
			if(ans[[2]]!='all')Mtest<-Mtest[givedim(ans[[2]]),]
			cat<-length(CLA$lev)
			numcat<-NULL
			gr<-NULL
			if(ans[[4]]!='None'){
				gr<-Mtest[,as.integer(ans[[4]])]
				for (i in 1:cat){
					numcat[i]<-sum(gr==CLA$lev[i])
				}
			}
			Mtest<-Mtest[,givedim(ans[[3]])]
			vv<-ncol(Mtest)
			rr<-nrow(Mtest)
			d2test<-matrix(rep(0,rr*cat),rr,cat)
			for (i in 1:rr){
				for (j in 1:cat){
					d2test[i,j]<-as.matrix(Mtest[i,]-CLA$means[j,])%*%matrix.inverse(CLA$cov)%*%as.matrix(t(Mtest[i,]-CLA$means[j,]))
				}
			}
			dd<-t(d2test)
			CLA$Md.pre<-dd
			CLA$lb.obj.pre<-unlist(dimnames(Mtest)[1])
			if(!is.null(gr)){ #test prediction
				print('Confusion Matrix in Prediction',quote=FALSE)
				grt<-CLA$lev[apply(dd,2,which.min)]
				mt<-matrix(rep(0,cat^2),cat,cat)
				mt<-as.data.frame(mt)
				names(mt)<-CLA$lev
				rownames(mt)<-CLA$lev
				for(i in 1:cat){
					for(j in 1:cat){
						for(k in 1:rr){
							if((CLA$lev[i]==gr[k])&(CLA$lev[j]==grt[k]))mt[i,j]<-mt[i,j]+1
						}
					}
				}
				print(mt,quote=FALSE)
				diag(mt)<-0
				errcat<-apply(mt,1,sum)
				print('Labels of Samples with wrong assignment',quote=FALSE)
				print(row.names(Mtest)[which(CLA$lev[apply(dd,2,which.min)]!=gr)],quote=FALSE)
				print('% Correct Predictions',quote=FALSE)
				errcat<-errcat/numcat
				errcat[is.nan(errcat)]<-0
				print(format(100*(1-errcat),digits=4),quote=FALSE)
				print('% Total Correct Predictions',quote=FALSE)
				print(format(100*(1-mean(errcat)),digits=4),quote=FALSE)
			}else{ #pure prediction
				print('Category Prediction',quote=FALSE)
				mt<-CLA$lev[apply(dd,2,which.min)]
				mt<-matrix(mt,length(mt),1)
				dimnames(mt)<-list(rownames(Mtest),'')
				print(mt,quote=FALSE)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
	}
}
CL_prediction_QDA<-function(previous.name=''){
	if(exists("CLA",envir=.GlobalEnv)){
		get("CLA",envir=.GlobalEnv)
		ans<-inpboxe4(c('*Matrix Name with samples to be predicted',
				'*Rows to be selected (e.g., 1:10,15)',
				'*X-Variables to be selected (e.g., 1:4,8)',
				'Category Variable'),c(previous.name,'all','','None'))
		if(!is.null(ans)){
			name<-ans[[1]]
			Mtest<-givemat(name)
			assign('previous.name',name,envir=.GlobalEnv)
			Mtest<-as.data.frame(Mtest)
			if(ans[[2]]!='all')Mtest<-Mtest[givedim(ans[[2]]),]
			cat<-length(CLA$lev)
			numcat<-NULL
			gr<-NULL
			if(ans[[4]]!='None'){
				gr<-Mtest[,as.integer(ans[[4]])]
				for (i in 1:cat){
					numcat[i]<-sum(gr==CLA$lev[i])
				}
			}
			Mtest<-Mtest[,givedim(ans[[3]])]
			vv<-ncol(Mtest)
			rrt<-nrow(Mtest)
			M<-CLA$dataset
			rr<-nrow(M)
			cc<-ncol(M)
			group<-CLA$group
			numcat<-NULL
			if(!is.null(gr)){
				for (i in 1:cat){
					numcat[i]<-sum(gr==CLA$lev[i])
				}
			}
			d2test<-matrix(rep(0,rrt*cat),rrt,cat)
			co<-matrix(rep(0,vv^2),vv,vv)
			me<-matrix(rep(0,vv*cat),cat,vv)
			for(j in 1:cat){
				me[j,]<-apply(M[group==CLA$lev[j],],2,mean)
				co<-cov(M[group==CLA$lev[j],])
				for(i in 1:rrt){
					d2test[i,j]<-as.matrix(Mtest[i,]-me[j,])%*% matrix.inverse(co)%*%t(as.matrix(Mtest[i,]-me[j,]))
				}
			}
			dd<-t(d2test)
			CLA$Md.pre<-dd
			CLA$lb.obj.pre<-unlist(dimnames(Mtest)[1])
			if(!is.null(gr)){ #test prediction
				print('Confusion Matrix in Prediction',quote=FALSE)
				grt<-CLA$lev[apply(dd,2,which.min)]
				mt<-matrix(rep(0,cat^2),cat,cat)
				mt<-as.data.frame(mt)
				names(mt)<-CLA$lev
				rownames(mt)<-CLA$lev
				for(i in 1:cat){
					for(j in 1:cat){
						for(k in 1:rrt){
							if((CLA$lev[i]==gr[k])&(CLA$lev[j]==grt[k]))mt[i,j]<-mt[i,j]+1
						}
					}
				}
				print(mt,quote=FALSE)
				diag(mt)<-0
				errcat<-apply(mt,1,sum)
				print('Labels of Samples with wrong assignment',quote=FALSE)
				print(row.names(Mtest)[which(CLA$lev[apply(dd,2,which.min)]!=gr)],quote=FALSE)
				print('% Correct Predictions',quote=FALSE)
				errcat<-errcat/numcat
				errcat[is.nan(errcat)]<-0
				print(format(100*(1-errcat),digits=4),quote=FALSE)
				print('% Total Correct Predictions',quote=FALSE)
				print(format(100*(1-mean(errcat)),digits=4),quote=FALSE)
			}else{ #pure prediction
				print('Category Prediction',quote=FALSE)
				mt<-CLA$lev[apply(dd,2,which.min)]
				mt<-matrix(mt,length(mt),1)
				dimnames(mt)<-list(rownames(Mtest),'')
				print(mt,quote=FALSE)
			}
		}
	}else{
		tk_messageBox(type=c("ok"),message="Run one Classification Method first!",caption="Input Error")
	}
}