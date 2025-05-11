# CARIPARO Project - Paola Sessa Lab

## Unraveling the Unique Role of the Embodied Route for Emotional Face Processing

Welcome to the CARIPARO project repository conducted by the Paola Sessa Lab at the University of Padova. In this project, we focused on investigating the process of emotional face processing.

## Experiment: Behavioral Moebius Syndrome

Description: The experiment involves an emotion recognition task using the Geneva Emotion Wheel (GEW) developed by [Scherer](https://doi.org/10.1177/0539018405058216) (2005). The analysis is performed using the *bpnreg* library from [Cremers](https://doi.org/10.3389/fpsyg.2018.02040) (2018). The analysis codes are developed by Dr. Thomas Quettier, PhD. You can reach him at thomas.quettier@unipd.it for further information.

### Experiment Information

- Stimuli: The videos were selected from the Amsterdam Dynamic Facial Expression Set ([ADFES](https://aice.uva.nl/research-tools/adfes-stimulus-set/adfes-stimulus-set.html)) and the Jerusalem Facial Expressions of Emotion ([JeFEE](https://netagabsi.wixsite.com/jefeeset)). We used 4 different identities (2 females and 2 males) for each dataset (ADFES: F01, F03, M02, M03; JeFEE: JF1, JF2, JM1, JM2), including all 6 basic facial expressions (happy, angry, afraid, disgusted, sad, surprised) plus neutral. The .mpeg video files were converted to .wmv format using VideoProc Version 4.1 Copyright © 2021 Digiarty.

- Number of trials: 8 identities (4 for each dataset) x 7 expressions = 56 x 1 cycle = Total 56 trials x 2 blocks = 112 trials

- Paradigm: Participants were instructed to indicate the expressed/felt emotion in each video clip using the Geneva Emotion Wheel. Wheel 1 represents the main recognized emotion, while Wheel 2 represents a possible secondary recognized emotion.

## Access to Analysis Codes

To access the analysis codes for the experiment, please download all the repository and begin with the *MBScontrol.Rproj* then open **main_script.R**.

Thank you for your interest in the CARIPARO project!

For more information, please contact Professor Paola Sessa at Paola.sessa@unipd.it.

## Access to report
- [Accuracy report](https://github.com/Merluin/Face_To_Face/blob/main/MBS_control/docs/Report_Accuracy.html)
- [Perceived intensity report](https://github.com/Merluin/Face_To_Face/blob/main/MBS_control/docs/Report_Perceived_intensity.html)



## References

Cremers, Jolien, and Irene Klugkist. 2018. “One Direction? A Tutorial for Circular Data Analysis Using R with Examples in Cognitive Psychology.” Front. Psychol. 9 (October): 2040. https://doi.org/10.3389/fpsyg.2018.02040.

Scherer, Klaus R. 2005. “What Are Emotions? And How Can They Be Measured?” Soc. Sci. Inf. 44 (4): 695–729. https://doi.org/10.1177/0539018405058216.

## (Collaborators) Please precede your commit's message with one or more of the following:
- BF: bug fix
- FF: feature fix. This is for fixes to code that hasn’t been released
- RF: refactoring
- NF: new feature
- ENH: enhancement (improvement to existing code)
- DOC: for all kinds of documentation-related commits
- TEST: for adding or changing tests
