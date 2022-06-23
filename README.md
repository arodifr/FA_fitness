# Human fluctuating asymmetry and fitness

Source code to generate all the analyzes in section 2.4 of the article "Formal models for the study of the relationship between fluctuating asymmetry and fitness in humans", accepted for publication in the American Journal of Biological Anthropology.

This article evaluates three of the main verbal models that have been proposed to explain the relationship between fluctuating asymmetry and fitness in humans: the “good genes”, the “good development”, and the “growth” hypotheses. To link theory with evidence on this topic, I used formal models (Smaldino, 2020) to make the research question explicit, as well as the assumptions about what components are (or are not) relevant and how they connect to each other. Specifically, based on the literature, I outlined a theoretical causal model and the theoretical object of inquiry for each verbal model (Figure 1). 

![This is an image](https://github.com/arodifr/FA_fitness/blob/main/Fig1_text.png)


Then, for each verbal model, I defined an empirical causal model and the targets of inference using observational data of facial asymmetries and life-history traits related to fitness (Figure 2).  

![This is an image](https://github.com/arodifr/FA_fitness/blob/main/Fig2_texto.png)

Finally, I used generalized linear models and causal inference as the estimation strategy.

![This is an image](https://github.com/arodifr/FA_fitness/blob/main/causal_models_results.png)


Note that due to the nature of the pedigree data, the data used for the heritability estimates are not available.  



#### Reference
Smaldino, P. (2020). How to translate a verbal theory into a formal model. Social Psychology,
51(4): 207-218. (doi: 10.1027/1864-9335/a000425)

Rohrer, J. (2018). Thinking clearly about correlations and causation: graphical causal models for
observational data. Advances in Methods and Practices in Psychological Science, 1(1): 27-42. (doi:
10.1177/2515245917745629)

