# Volcano plot Shiny operator for Tercen

##### Description

Launches a Shiny application for a customizable volcano plot.

##### Usage

Input projection|.
---|---
`x-axis`        | log2 fold change (numeric)
`y-axis`        | negative log10 of the p-values adjusted for multiple testing (numeric)
`labels`        | the gene names to be displayed on the plot (character)

Output relations|.
---|---
`Operator view`        | view of the Shiny application

##### Details

Volcano plots show the significance (typically negative log10 of the p-values adjusted 
for multiple testing, y-axis) against the change in gene expression 
(log2 fold change, x-axis). Additionally, the genes can be labeled using a 
gene name variable mapped onto 'labels'. 

##### See Also

[template_R_operator](https://github.com/tercen/template_R_operator)
