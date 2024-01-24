library(RColorBrewer)
library("gplots")

# Proteomics All
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\1.Proteomics ALL\csv\Cellular Movement.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\1.Proteomics ALL\csv\GI Disease.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\1.Proteomics ALL\csv\ID.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\1.Proteomics ALL\csv\Immune Cell Trafficking.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\1.Proteomics ALL\csv\Inflammatory Response.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\1.Proteomics ALL\csv\Respiratory Disease.csv)")

# Proteomics Men
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\2.Proteomics MEN\csv\Cellular Movement.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\2.Proteomics MEN\csv\GI Disease.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\2.Proteomics MEN\csv\ID.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\2.Proteomics MEN\csv\Immune Cell Trafficking.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\2.Proteomics MEN\csv\Inflammatory Response.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\2.Proteomics MEN\csv\Respiratory Disease.csv)")

# Olink All
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\3.OLINK ALL subjects\csv\Cellular Movement.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\3.OLINK ALL subjects\csv\GI Dz.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\3.OLINK ALL subjects\csv\ID.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\3.OLINK ALL subjects\csv\Immune Cell Trafficking.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\3.OLINK ALL subjects\csv\Inflammatory Response.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\3.OLINK ALL subjects\csv\Resp Dz.csv)")
# Olink Men
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\4.OLINK MEN\csv\Cellular Movement.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\4.OLINK MEN\csv\GI Disease.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\4.OLINK MEN\csv\ID.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\4.OLINK MEN\csv\Immune Cell Trafficking.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\4.OLINK MEN\csv\Inflammatory Response.csv)")
cell_movement <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Data_R3\IPA results\4.OLINK MEN\csv\Respiratory Dz.csv)")

cell_movement <- na.omit(cell_movement)
level_order <- cell_movement$Diseases.or.Functions.Annotation
mid<-mean(cell_movement$Activation.z.score)
cell_movement %>%
  arrange(corneglogpvalue) %>% #neglogpval
  mutate(name=factor(Diseases.or.Functions.Annotation, levels=Diseases.or.Functions.Annotation)) %>%
ggplot(aes(x = factor(Diseases.or.Functions.Annotation, level = level_order),
           y = corneglogpvalue, #neglogpval
           color = Activation.z.score, 
           size = X..Molecules)) + 
  geom_point(alpha=0.75) + 
  ylab("") + 
  xlab("") + 
  ggtitle("") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1, size =8, face = "bold", colour = "black")) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.3, hjust=1, size =8, face = "bold", colour = "black")) +
  theme(plot.title = element_text(size = 10, face = "plain"),
        legend.title=element_text(size=8, face = "plain"), 
        legend.text=element_text(size=8, face = "plain")) +
  theme(#panel.grid.minor=element_blank(),
        #panel.grid.major=element_blank()
    ) +
  coord_flip() +
  scale_size_continuous(
    breaks = c(10, 60),
    limits = c(0, 220)) +
  scale_x_discrete(limits=rev) + 
  scale_color_gradient2(low = "blue", 
                        #mid="yellow", 
                        high="red", space ="Lab" )

#scale_color_gradient2(low = "blue", mid="yellow", high="red", space ="Lab" )

# Heatmap
heatmap.data <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\Olink data for heat map analysis\cyto.csv)", row.names = 1)
heatmap.data <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Latest data from the omics core used for heatmap\data ready for heatmap_R.csv)", row.names=1)

# DATA ANALYSIS: HEATMAPS ####
# Hierarchical clustering with heatmaps
## Get some nicer colours
mypalette <- brewer.pal(11,"RdYlGn")
morecols <- colorRampPalette(mypalette)

# Set up colour vector for celltype variable
my_palette <- colorRampPalette(c("dodgerblue", "white", "darkred"))(n = 299)
col.cell <- c('lightblue', 'blue', 'red')

MS.heatmap.matrix <- data.matrix(heatmap.data)
heatmap.plot <- heatmap.2(MS.heatmap.matrix,
          #col=rev(morecols(50)),
          col=rev(my_palette),
          trace="none", 
          main="RPPA",
          #ColSideColors=col.cell,
          dendrogram = 'row',
          #Rowv = FALSE,
          Colv = FALSE,
          #key = NULL,
          key.title = "Z-Score",
          scale="row",
          sepwidth=c(0.2,0.2),
          #rowsep = c(2, 5, 9, 10, 12, 15, 17, 20, 21),
          margins = c(6, 6),
          cexRow = c(0.05), # row font size
          cexCol = c(0.5)) # col font size
heatmap.plot

# Load data
heatmap.data2 <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Latest data from the omics core used for heatmap\avg by sex and tx Men data ready for heatmap.csv)", row.names = 1)
heatmap.data2 <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\Olink data for heat map analysis\men only for heatmap.csv)", row.names = 1)

# Matrix
MS.heatmap.matrix2 <- data.matrix(heatmap.data2)

# Unordered
# Now plot heatmaps for sex stratified data based on sequence above
heatmap.plot2 <- heatmap.2(MS.heatmap.matrix2,
          #col=rev(morecols(50)),
          col=rev(my_palette),
          trace="none", 
          main="RPPA",
          #ColSideColors=col.cell,
          dendrogram = "none",
          #Rowv = FALSE,
          Colv = FALSE,
          #key = NULL,
          key.title = "Z-Score",
          scale="row",
          sepwidth=c(0.2,0.2),
          #rowsep = c(2, 5, 9, 10, 12, 15, 17, 20, 21),
          margins = c(6, 6),
          cexRow = c(0.05), # row font size
          cexCol = c(0.5)) # col font size

# Make second matrix match row order of first
reorder_mat <- MS.heatmap.matrix2[rev(heatmap.plot$rowInd),]

# Ordered
# Now plot heatmaps for sex stratified data based on sequence above
heatmap.2(reorder_mat,
                           #col=rev(morecols(50)),
                           col=rev(my_palette),
                           trace="none", 
                           main="RPPA",
                           #ColSideColors=col.cell,
                           dendrogram = "none",
                           Rowv = FALSE,
                           Colv = FALSE,
                           #key = NULL,
                           key.title = "Z-Score",
                           scale="row",
                           sepwidth=c(0.2,0.2),
                           #rowsep = c(2, 5, 9, 10, 12, 15, 17, 20, 21),
                           margins = c(6, 6),
                           cexRow = c(0.05), # row font size
                           cexCol = c(0.5)) # col font size

## Female #
# Load data
heatmap.data2 <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Latest data from the omics core used for heatmap\avg by sex and tx Women data ready for heatmap.csv)", row.names = 1)
heatmap.data2 <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\Olink data for heat map analysis\women only for heatmap.csv)", row.names = 1)

# Matrix
MS.heatmap.matrix2 <- data.matrix(heatmap.data2)

# Unordered
# Now plot heatmaps for sex stratified data based on sequence above
heatmap.plot2 <- heatmap.2(MS.heatmap.matrix2,
                           #col=rev(morecols(50)),
                           col=rev(my_palette),
                           trace="none", 
                           main="RPPA",
                           #ColSideColors=col.cell,
                           dendrogram = "none",
                           #Rowv = FALSE,
                           Colv = FALSE,
                           #key = NULL,
                           key.title = "Z-Score",
                           scale="row",
                           sepwidth=c(0.2,0.2),
                           #rowsep = c(2, 5, 9, 10, 12, 15, 17, 20, 21),
                           margins = c(6, 6),
                           cexRow = c(0.05), # row font size
                           cexCol = c(0.5)) # col font size

# Make second matrix match row order of first (make sure csv is alphabetically ordered for both male and female, or order in R)
reorder_mat <- MS.heatmap.matrix2[rev(heatmap.plot$rowInd),]

# Ordered
# Now plot heatmaps for sex stratified data based on sequence above
heatmap.2(reorder_mat,
          #col=rev(morecols(50)),
          col=rev(my_palette),
          trace="none", 
          main="RPPA",
          #ColSideColors=col.cell,
          dendrogram = "none",
          Rowv = FALSE,
          Colv = FALSE,
          #key = NULL,
          key.title = "Z-Score",
          scale="row",
          sepwidth=c(0.2,0.2),
          #rowsep = c(2, 5, 9, 10, 12, 15, 17, 20, 21),
          margins = c(6, 6),
          cexRow = c(0.05), # row font size
          cexCol = c(0.5)) # col font size


# Load data OLINK
heatmap.data <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\Olink data\plotdata_cyto.csv)", row.names = 1)

# DATA ANALYSIS: HEATMAPS ####
# Hierarchical clustering with heatmaps
## Get some nicer colours
mypalette <- brewer.pal(11,"RdYlGn")
morecols <- colorRampPalette(mypalette)

# Set up colour vector for celltype variable
my_palette <- colorRampPalette(c("darkred", "white", "dodgerblue"))(n = 299)
col.cell <- c('lightblue', 'blue', 'red')

MS.heatmap.matrix <- data.matrix(heatmap.data)
heatmap.2(MS.heatmap.matrix,
          #col=rev(morecols(50)),
          col=rev(my_palette),
          trace="none", 
          main="RPPA",
          #ColSideColors=col.cell,
          dendrogram = 'row',
          #Rowv = FALSE,
          Colv = FALSE,
          #key = NULL,
          key.title = "Z-Score",
          scale="row",
          sepwidth=c(0.2,0.2),
          #rowsep = c(2, 5, 9, 10, 12, 15, 17, 20, 21),
          margins = c(6, 6),
          cexRow = c(0.5), # row font size
          cexCol = c(0.5)) # col font size

# Load data PROTEOMICS
heatmap.data <- read.csv(r"(C:\Users\mqadir\Box\Fahd shared to Dragana\E2P4_COVID study\Latest data from the omics core used for heatmap\alldata_plot.csv)", row.names = 1)

# DATA ANALYSIS: HEATMAPS ####
# Hierarchical clustering with heatmaps
## Get some nicer colours
mypalette <- brewer.pal(11,"RdYlGn")
morecols <- colorRampPalette(mypalette)

# Set up colour vector for celltype variable
my_palette <- colorRampPalette(c("dodgerblue", "white", "darkred"))(n = 299)
col.cell <- c('lightblue', 'blue', 'red')

MS.heatmap.matrix <- data.matrix(heatmap.data)
heatmap.2(MS.heatmap.matrix,
          #col=rev(morecols(50)),
          col=rev(my_palette),
          trace="none", 
          main="RPPA",
          #ColSideColors=col.cell,
          dendrogram = 'row',
          #Rowv = FALSE,
          Colv = FALSE,
          #key = NULL,
          key.title = "Z-Score",
          scale="row",
          sepwidth=c(0.2,0.2),
          #rowsep = c(2, 5, 9, 10, 12, 15, 17, 20, 21),
          margins = c(6, 6),
          cexRow = c(0.05), # row font size
          cexCol = c(0.5)) # col font size

> sessionInfo()
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

time zone: America/Chicago
tzcode source: internal

attached base packages:
[1] grid      stats4    stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] RColorBrewer_1.1-3                Pando_1.0.5                       DESeq2_1.42.0                    
 [4] BiocParallel_1.36.0               cicero_1.3.9                      Gviz_1.46.1                      
 [7] chromVAR_1.5.0                    motifmatchr_1.24.0                BSgenome.Hsapiens.UCSC.hg38_1.4.5
[10] BSgenome_1.70.1                   rtracklayer_1.62.0                BiocIO_1.12.0                    
[13] Biostrings_2.70.1                 XVector_0.42.0                    TFBSTools_1.40.0                 
[16] JASPAR2020_0.99.10                qs_0.25.7                         R.utils_2.12.3                   
[19] R.oo_1.25.0                       R.methodsS3_1.8.2                 devtools_2.4.5                   
[22] usethis_2.2.2                     ggVennDiagram_1.5.0               ggvenn_0.1.10                    
[25] DropletUtils_1.22.0               Nebulosa_1.12.0                   circlize_0.4.15                  
[28] ComplexHeatmap_2.18.0             viridis_0.6.4                     viridisLite_0.4.2                
[31] EnrichmentBrowser_2.32.0          graph_1.80.0                      escape_1.12.0                    
[34] dittoSeq_1.14.0                   DOSE_3.28.2                       clusterProfiler_4.10.0           
[37] MeSHDbi_1.38.0                    AnnotationHub_3.10.0              BiocFileCache_2.10.1             
[40] dbplyr_2.4.0                      org.Hs.eg.db_3.18.0               GOSemSim_2.28.1                  
[43] glmGamPoi_1.14.0                  EnhancedVolcano_1.20.0            DoubletFinder_2.0.4              
[46] future_1.33.1                     patchwork_1.2.0                   clustree_0.5.1                   
[49] ggraph_2.1.0                      plotly_4.10.4                     EnsDb.Hsapiens.v86_2.99.0        
[52] ensembldb_2.26.0                  AnnotationFilter_1.26.0           GenomicFeatures_1.54.1           
[55] AnnotationDbi_1.64.1              scDblFinder_1.16.0                Signac_1.11.0                    
[58] harmony_1.2.0                     monocle3_1.3.4                    SingleCellExperiment_1.24.0      
[61] SummarizedExperiment_1.32.0       GenomicRanges_1.54.1              GenomeInfoDb_1.38.5              
[64] IRanges_2.36.0                    S4Vectors_0.40.2                  MatrixGenerics_1.14.0            
[67] matrixStats_1.2.0                 Biobase_2.62.0                    BiocGenerics_0.48.1              
[70] SeuratObject_4.1.4                Seurat_4.3.0                      reticulate_1.34.0                
[73] data.table_1.14.10                lubridate_1.9.3                   forcats_1.0.0                    
[76] purrr_1.0.2                       readr_2.1.5                       tidyr_1.3.0                      
[79] tibble_3.2.1                      tidyverse_2.0.0                   dplyr_1.1.4                      
[82] ggridges_0.5.5                    Matrix_1.6-5                      cowplot_1.1.2                    
[85] Rcpp_1.0.12                       SoupX_1.6.2                       hdf5r_1.3.9                      
[88] stringr_1.5.1                     leiden_0.4.3.1                    ggrepel_0.9.5                    
[91] ggplot2_3.4.4                    

loaded via a namespace (and not attached):
  [1] igraph_1.6.0                  Formula_1.2-5                 ica_1.0-3                     scater_1.30.1                
  [5] maps_3.4.2                    zlibbioc_1.48.0               tidyselect_1.2.0              bit_4.0.5                    
  [9] doParallel_1.0.17             clue_0.3-65                   lattice_0.22-5                rjson_0.2.21                 
 [13] blob_1.2.4                    urlchecker_1.0.1              S4Arrays_1.2.0                dichromat_2.0-0.1            
 [17] parallel_4.3.2                seqLogo_1.68.0                png_0.1-8                     cli_3.6.2                    
 [21] ggplotify_0.1.2               ProtGenerics_1.34.0           goftest_1.2-3                 grr_0.9.5                    
 [25] bluster_1.12.0                BiocNeighbors_1.20.2          uwot_0.1.16                   shadowtext_0.1.3             
 [29] curl_5.2.0                    evaluate_0.23                 mime_0.12                     tidytree_0.4.6               
 [33] stringi_1.8.3                 backports_1.4.1               XML_3.99-0.16                 httpuv_1.6.13                
 [37] magrittr_2.0.3                rappdirs_0.3.3                splines_4.3.2                 RcppRoll_0.3.0               
 [41] mclust_6.0.1                  RApiSerialize_0.1.2           jpeg_0.1-10                   DT_0.31                      
 [45] sctransform_0.4.1             ggbeeswarm_0.7.2              sessioninfo_1.2.2             DBI_1.2.1                    
 [49] terra_1.7-65                  HDF5Array_1.30.0              withr_3.0.0                   rprojroot_2.0.4              
 [53] enrichplot_1.22.0             xgboost_1.7.6.1               lmtest_0.9-40                 GSEABase_1.64.0              
 [57] tidygraph_1.3.0               BiocManager_1.30.22           htmlwidgets_1.6.4             fs_1.6.3                     
 [61] biomaRt_2.58.0                labeling_0.4.3                SparseArray_1.2.3             annotate_1.80.0              
 [65] VariantAnnotation_1.48.1      zoo_1.8-12                    knitr_1.45                    TFMPvalue_0.0.9              
 [69] timechange_0.3.0              foreach_1.5.2                 ggpointdensity_0.1.0          fansi_1.0.6                  
 [73] caTools_1.18.2                ggtree_3.10.0                 rhdf5_2.46.1                  poweRlaw_0.70.6              
 [77] irlba_2.3.5.1                 ggrastr_1.0.2                 gridGraphics_0.5-1            ellipsis_0.3.2               
 [81] lazyeval_0.2.2                yaml_2.3.8                    survival_3.5-7                scattermore_1.2              
 [85] BiocVersion_3.18.1            crayon_1.5.2                  RcppAnnoy_0.0.21              mapproj_1.2.11               
 [89] progressr_0.14.0              tweenr_2.0.2                  later_1.3.2                   Rgraphviz_2.46.0             
 [93] base64enc_0.1-3               codetools_0.2-19              GlobalOptions_0.1.2           profvis_0.3.8                
 [97] KEGGREST_1.42.0               Rtsne_0.17                    shape_1.4.6                   limma_3.58.1                 
[101] Rsamtools_2.18.0              filelock_1.0.3                foreign_0.8-86                pkgconfig_2.0.3              
[105] KEGGgraph_1.62.0              xml2_1.3.6                    GenomicAlignments_1.38.2      aplot_0.2.2                  
[109] biovizBase_1.50.0             spatstat.sparse_3.0-3         ape_5.7-1                     xtable_1.8-4                 
[113] interp_1.1-5                  plyr_1.8.9                    httr_1.4.7                    tools_4.3.2                  
[117] globals_0.16.2                pkgbuild_1.4.3                checkmate_2.3.1               htmlTable_2.4.2              
[121] beeswarm_0.4.0                broom_1.0.5                   nlme_3.1-164                  HDO.db_0.99.1                
[125] lme4_1.1-35.1                 digest_0.6.34                 farver_2.1.1                  tzdb_0.4.0                   
[129] reshape2_1.4.4                ks_1.14.2                     yulab.utils_0.1.3             rpart_4.1.23                 
[133] DirichletMultinomial_1.44.0   glue_1.7.0                    cachem_1.0.8                  polyclip_1.10-6              
[137] Hmisc_5.1-1                   generics_0.1.3                mvtnorm_1.2-4                 parallelly_1.36.0            
[141] pkgload_1.3.4                 statmod_1.5.0                 here_1.0.1                    ScaledMatrix_1.10.0          
[145] minqa_1.2.6                   pbapply_1.7-2                 nabor_0.5.0                   spam_2.10-0                  
[149] gson_0.1.0                    dqrng_0.3.2                   utf8_1.2.4                    gtools_3.9.5                 
[153] graphlayouts_1.1.0            gridExtra_2.3                 shiny_1.8.0                   GSVA_1.50.0                  
[157] GenomeInfoDbData_1.2.11       pals_1.8                      rhdf5filters_1.14.1           RCurl_1.98-1.14              
[161] memoise_2.0.1                 rmarkdown_2.25                pheatmap_1.0.12               scales_1.3.0                 
[165] RANN_2.6.1                    stringfish_0.16.0             spatstat.data_3.0-4           rstudioapi_0.15.0            
[169] cluster_2.1.6                 msigdbr_7.5.1                 spatstat.utils_3.0-4          hms_1.1.3                    
[173] fitdistrplus_1.1-11           munsell_0.5.0                 colorspace_2.1-0              FNN_1.1.4                    
[177] rlang_1.1.3                   DelayedMatrixStats_1.24.0     sparseMatrixStats_1.14.0      dotCall64_1.1-1              
[181] ggforce_0.4.1                 scuttle_1.12.0                xfun_0.41                     CNEr_1.38.0                  
[185] remotes_2.4.2.1               iterators_1.0.14              abind_1.4-5                   interactiveDisplayBase_1.40.0
[189] treeio_1.26.0                 Rhdf5lib_1.24.1               bitops_1.0-7                  promises_1.2.1               
[193] scatterpie_0.2.1              RSQLite_2.3.4                 qvalue_2.34.0                 fgsea_1.28.0                 
[197] DelayedArray_0.28.0           GO.db_3.18.0                  compiler_4.3.2                prettyunits_1.2.0            
[201] boot_1.3-28.1                 beachmat_2.18.0               listenv_0.9.0                 edgeR_4.0.10                 
[205] BiocSingular_1.18.0           tensor_1.5                    MASS_7.3-60.0.1               progress_1.2.3               
[209] UCell_2.6.2                   babelgene_22.9                spatstat.random_3.2-2         R6_2.5.1                     
[213] fastmap_1.1.1                 fastmatch_1.1-4               vipor_0.4.7                   ROCR_1.0-11                  
[217] nnet_7.3-19                   rsvd_1.0.5                    gtable_0.3.4                  KernSmooth_2.23-22           
[221] latticeExtra_0.6-30           miniUI_0.1.1.1                deldir_2.0-2                  htmltools_0.5.7              
[225] RcppParallel_5.1.7            bit64_4.0.5                   spatstat.explore_3.2-5        lifecycle_1.0.4              
[229] nloptr_2.0.3                  restfulr_0.0.15               vctrs_0.6.5                   VGAM_1.1-9                   
[233] spatstat.geom_3.2-7           scran_1.30.1                  ggfun_0.1.4                   sp_2.1-2                     
[237] pracma_2.4.4                  future.apply_1.11.1           pillar_1.9.0                  metapod_1.10.1               
[241] locfit_1.5-9.8                jsonlite_1.8.8                GetoptLong_1.0.5  

