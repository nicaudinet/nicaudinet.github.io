---
layout: post
title: "What to do about LLM bias?"
excerpt: Thoughts and opinions about bias in Large Language Models 
---

# What to do about LLM bias?

Why should we care about LLM bias in the first place?

## Why are LLMs biased?

Position Paper: [Large Language Models Are Biased Because They Are Large Language
Models](https://arxiv.org/abs/2406.13138)

LLMs do a great job at learning the distribution of the corpus they are trained
on. That's their explicit learning objective! Radical distributionalism: meaning
is nothing but corpus distribution.

If the training data is biased, then the LLM will also be biased. The LLM cannot
tell the difference between biased and unbiased text.

In his paper he illustrates this with the following example. Consider the three
following sentences:

1) A nurse is a kind of healthcare worker.

2) A nurse is likely to wear blue clothing at work.

3) A nurse is likely to wear a dress to a formal occasion.

All three sentences are statistically true at present. The first sentence is
true by definition, independently of context, and is socially acceptable. The
second sentence is true but little about being a nurses makes it so that they
have to wear blue. The second sentence is also socially acceptable, in the sense
that we are OK with using the colour of clothing to distinguish nurses from
other workers. The third sentence is contingently true in the same sense as the
second one, but it is considered socially unacceptable to distinguish nurses
from other workers based on whether they are likely to wear a dress on formal
occasions.

As they are currently trained, LLMs cannot distinguish between these three
sentences. It just trains to learn the corpus distribution: the more frequently
the same sequence of words appears in the corpus, the more likely it is to learn
it. They cannot distinguish between definitions and contingent facts, and
between what is socially acceptable and what is not.

The biases learned by a model are difficult to measure and even more difficult
to generalise:
- [Emergent Misalignment: Narrow finetuning can produce broadly misaligned LLMs](https://arxiv.org/pdf/2502.17424)
- [Bias in Langauge Models: Beyond Trick Tests and Towards RUTEd
Evaluation](https://aclanthology.org/2025.acl-long.7/)
- [Dialect prejudice predicts AI decisions about people's character,
employability, and criminality](https://arxiv.org/abs/2403.00742)

If these biases are hard to measure then they are even more difficult to fix.
Post-training methods like RLHF can patch LLMs on a surface level but it's
unclear how deep they go. And even if they did, they would at best just
substitute biases in the corpus distribution with biases from the small group of
people in charge of the post-training.

## On the importance of interpretability

On the importance of interpretability in language models ...

## Where to go from here

So where to go from here? In my opinion there are a few interesting directions
to take:

### Extracting small tools from LLMs

- Give up on LLMs as general tools for everything. Instead, focus on extracting
smaller, interpretable, task-specific tools from the LLM.
- Smaller, more task-oriented systems are more likely to be fully understood,
qualitatively or even formally.
- Mechanistic interpretability is a step towards this but they don't go far
enough in giving up on LLMs as general intelligence.
    - Interesting example: the [SHIFT
    method](https://arxiv.org/pdf/2403.19647?).
- Once the circuit / program / tool is extracted and interpreted, then we can
make normative decisions by inspecting it and modifying it as we like.
- This approach becomes more powerful the easier and more accessible we make the
extraction and interpretation process.


### Intrinsically interpretable models

- [Learning Transformer
Programs](https://proceedings.neurips.cc/paper_files/paper/2023/file/995f693b73050f90977ed2828202645c-Paper-Conference.pdf)
and the RASP approach could be one way of getting more interpretable models.
- There is some evidence that [NEAT-style evolutionary training
methods](https://arxiv.org/abs/2505.11581) with more human feedback at training
time could also help align neurons within the model with human-understandable
features.
- How do these methods scale though? What is the performance cost and is it
worth it?

### Neuro-Symbolic approaches

With some Neuro-Symbolic approaches one could imagine that the system becomes
partially interpretable (or at least "checkable" by some other system) due to
its symbolic components.
- [DreamCoder](https://dl.acm.org/doi/pdf/10.1145/3453483.3454080) - can see
what abstractions it's using to generate programs
