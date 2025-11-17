# TinyTransformer Representation Formats Guide

This guide provides a comprehensive overview of all representation formats available for the TinyTransformer model, including the newly added AIML, OpenCog AtomSpace, and TOML Hypergraph formats.

## Overview

The TinyTransformer model is available in **15+ different formats**, each optimized for different use cases. This diversity enables the model to integrate with various tools, frameworks, and cognitive architectures.

## Representation Categories

### 1. Binary Formats (Inference-Optimized)

#### GGUF Format
- **File**: `tiny_model.gguf`
- **Size**: ~2KB
- **Best for**: Production inference with llama.cpp
- **Features**: Complete model with metadata, vocabulary, and weights in binary format

#### PyTorch Binary
- **File**: `tiny_transformer.pth`
- **Size**: ~6KB
- **Best for**: PyTorch training and fine-tuning
- **Features**: State dict with all layer weights

#### ONNX
- **File**: `tiny_transformer.onnx`
- **Size**: ~15KB
- **Best for**: Cross-platform deployment
- **Features**: Optimized computation graph

#### Binary Weights
- **File**: `tiny_model_weights.bin`
- **Size**: 800 bytes
- **Best for**: Separate weight storage
- **Features**: Raw float32 weights only

---

### 2. Human-Readable Formats

#### JSON Representations

##### Full JSON
- **File**: `tiny_model_gguf.json`
- **Size**: ~4KB
- **Best for**: Debugging and inspection
- **Features**: Complete model with all weights in JSON arrays

##### Parsed JSON
- **File**: `tiny_model_gguf_parsed.json`
- **Size**: ~300 bytes
- **Best for**: Quick metadata inspection
- **Features**: Metadata and hyperparameters only

#### TOML Representations

##### Standard TOML with Weights
- **File**: `tiny_model_gguf_with_weights.toml`
- **Size**: ~5KB
- **Best for**: Manual weight editing
- **Features**: Node-edge structure with weight arrays

##### Test TOML
- **File**: `tiny_model_gguf_test.toml`
- **Size**: ~400 bytes
- **Best for**: Architecture testing
- **Features**: Structure without weights

##### **NEW: TOML Hypergraph**
- **File**: `representations/tiny_transformer_hypergraph.toml`
- **Size**: ~15KB
- **Best for**: Explicit hypergraph specification
- **Features**: 
  - Metadata section with model configuration
  - Vertices with types, shapes, and properties
  - Hyperedges with explicit source/target tuples
  - Full weight matrices
  - 18 vertices (tensors and parameters)
  - 9 hyperedges (operations)

**Example:**
```toml
[metadata]
model_name = "TinyTransformer"
representation = "hypergraph"

[vertices.input_tokens]
type = "tensor"
shape = ['batch', 5]

[hyperedges.embed]
operation = "embedding_lookup"
sources = ["input_tokens", "embedding_weights"]
targets = ["embeddings"]

[weights.embedding_weights]
values = [[...], [...]]
```

---

### 3. Graph Representations

#### Hypergraph (JSON)
- **File**: `representations/tiny_transformer_hypergraph.json`
- **Size**: ~20KB
- **Best for**: Multi-way relationship analysis
- **Features**:
  - Vertices representing tensors and operations
  - Hyperedges connecting multiple inputs to outputs
  - Most expressive format for transformer operations
  - Captures complex dependencies like attention

#### DAG (JSON)
- **File**: `representations/tiny_transformer_dag.json`
- **Size**: ~15KB
- **Best for**: Sequential computation flow
- **Features**:
  - Nodes for operations and tensors
  - Directed edges showing data flow
  - Topological ordering support

#### GraphViz DOT Files
- **Files**: `*.dot`
- **Best for**: Visualization
- **Features**: Can be rendered with Graphviz tools

---

### 4. Symbolic/Mathematical Representations

#### Symbolic JSON
- **File**: `representations/tiny_transformer_symbolic.json`
- **Size**: ~8KB
- **Best for**: Mathematical analysis
- **Features**: Parameters and expressions as equations

#### Symbolic Markdown
- **File**: `representations/tiny_transformer_symbolic.md`
- **Best for**: Documentation
- **Features**: Human-readable equations

#### Symbolic LaTeX
- **File**: `representations/tiny_transformer_symbolic.tex`
- **Best for**: Academic papers
- **Features**: Proper mathematical typesetting

---

### 5. Chatbot/AI Agent Formats

#### **NEW: AIML (Pandorabot)**
- **File**: `representations/tiny_transformer.aiml`
- **Size**: ~4.5KB
- **Best for**: Chatbot integration, educational interfaces
- **Features**:
  - 23 pattern-response categories
  - Architecture queries
  - Layer descriptions
  - Operation explanations
  - Conversational interface

**Example Usage:**
```xml
<category>
  <pattern>WHAT IS THE EMBEDDING DIMENSION</pattern>
  <template>The embedding dimension is 5.</template>
</category>
```

**Use cases:**
- Create chatbots that explain the model
- Interactive model documentation
- Educational demonstrations
- Pattern-based knowledge retrieval
- Integration with Pandorabots platform

---

### 6. Cognitive Architecture Formats

#### **NEW: OpenCog AtomSpace**
- **File**: `representations/tiny_transformer_atomspace.scm`
- **Size**: ~6.4KB
- **Best for**: Symbolic AI, cognitive architectures, neuro-symbolic systems
- **Features**:
  - **50+ atoms** representing model structure
  - **ConceptNodes**: Model concepts and layers
  - **PredicateNodes**: Properties and relationships
  - **InheritanceLinks**: Type hierarchies
  - **EvaluationLinks**: Property assertions with truth values (PLN)
  - **ExecutionLinks**: Computational flow
  - **5 knowledge base rules** for inference
  - Compatible with:
    - **URE** (Unified Rule Engine) - Pattern matching and rule application
    - **PLN** (Probabilistic Logic Networks) - Uncertain reasoning
    - **ECAN** (Economic Attention Networks) - Attention allocation
    - **MOSES** (Meta-Optimizing Semantic Evolutionary Search) - Optimization

**Example Structure:**
```scheme
(use-modules (opencog) (opencog ure) (opencog pln))

; Model as concept with truth value
(ConceptNode "TinyTransformer" (stv 1.0 1.0))

; Type hierarchy
(InheritanceLink (stv 1.0 1.0)
  (ConceptNode "TinyTransformer")
  (ConceptNode "TransformerModel"))

; Properties
(EvaluationLink (stv 1.0 1.0)
  (PredicateNode "hasVocabularySize")
  (ListLink
    (ConceptNode "TinyTransformer")
    (ConceptNode "VocabSize10")))

; Computational flow
(ExecutionLink
  (SchemaNode "Embed")
  (ListLink
    (ConceptNode "InputTokens")
    (ConceptNode "Embeddings")))
```

**Use cases:**
- Symbolic reasoning about neural models
- Neuro-symbolic AI systems
- Probabilistic inference over model structure
- Evolutionary optimization
- Integration with OpenCog cognitive architecture
- Multi-agent AI systems
- Explainable AI through logical reasoning

---

## Format Comparison Matrix

| Format | Size | Readable | Weights | Graph | Symbolic | AI Agent | Best Use Case |
|--------|------|----------|---------|-------|----------|----------|---------------|
| GGUF | 2KB | No | Yes | No | No | No | Production inference |
| PyTorch | 6KB | No | Yes | No | No | No | Training/Fine-tuning |
| ONNX | 15KB | No | Yes | Yes | No | No | Cross-platform deployment |
| JSON Full | 4KB | Yes | Yes | No | No | No | Debugging |
| JSON Parsed | 300B | Yes | No | No | No | No | Quick inspection |
| TOML Standard | 5KB | Yes | Yes | No | No | No | Manual editing |
| TOML Hypergraph | 15KB | Yes | Yes | Yes | No | No | Config-based hypergraph |
| Hypergraph JSON | 20KB | Yes | No | Yes | No | No | Graph analysis |
| DAG JSON | 15KB | Yes | No | Yes | No | No | Flow analysis |
| Symbolic JSON | 8KB | Yes | No | No | Yes | No | Math analysis |
| Symbolic LaTeX | - | Yes | No | No | Yes | No | Academic papers |
| AIML | 4.5KB | Yes | No | No | No | Yes | Chatbot integration |
| OpenCog AtomSpace | 6.4KB | Partial | No | Yes | Yes | Yes | Cognitive AI |

---

## Generating Representations

### Generate Standard Representations
```bash
python examples/generate_representations.py
```

This generates:
- Hypergraph JSON and DOT
- DAG JSON and DOT
- Symbolic JSON, Markdown, and LaTeX

### Generate Additional Representations
```bash
python examples/generate_additional_representations.py
```

This generates:
- AIML XML and JSON
- OpenCog Scheme and JSON
- TOML Hypergraph and JSON

---

## Python API Examples

### AIML
```python
from gguf_workbench.representations import AIMLRepresentation

# Create from model
aiml = AIMLRepresentation.from_tiny_transformer()

# Save formats
aiml.save_xml('model.aiml')
aiml.save_json('model_aiml.json')

# Inspect
print(f"Categories: {len(aiml.categories)}")
for cat in aiml.categories[:3]:
    print(f"  {cat['pattern']} -> {cat['template']}")
```

### OpenCog AtomSpace
```python
from gguf_workbench.representations import OpenCogAtomSpaceRepresentation

# Create from model
atomspace = OpenCogAtomSpaceRepresentation.from_tiny_transformer()

# Save formats
atomspace.save_scheme('model.scm')
atomspace.save_json('model_atomspace.json')

# Inspect
print(f"Atoms: {len(atomspace.atoms)}")
print(f"KB Rules: {len(atomspace.knowledge_base)}")
print(atomspace.to_scheme()[:500])  # Preview
```

### TOML Hypergraph
```python
from gguf_workbench.representations import TOMLHypergraphRepresentation

# Create from model
toml_hg = TOMLHypergraphRepresentation.from_tiny_transformer(
    include_weights=True
)

# Save formats
toml_hg.save_toml('model_hypergraph.toml')
toml_hg.save_json('model_hypergraph.json')

# Inspect
stats = toml_hg.to_json()['statistics']
print(f"Vertices: {stats['vertex_count']}")
print(f"Hyperedges: {stats['hyperedge_count']}")
print(f"Parameters: {stats['parameter_count']}")
```

---

## Integration Examples

### Using AIML with Pandorabots

1. Upload `tiny_transformer.aiml` to your Pandorabot
2. Chat with the bot:
   ```
   You: What is the architecture?
   Bot: This is a transformer architecture model.
   
   You: Describe the attention layer
   Bot: The self-attention layer has query, key, and value projections...
   ```

### Using OpenCog AtomSpace

```scheme
; Load the model
(load "tiny_transformer_atomspace.scm")

; Query the AtomSpace
(cog-get-atoms 'ConceptNode)  ; Get all concepts

; Run PLN inference
(pln-fc (Concept "TinyTransformer"))

; Pattern matching
(cog-bind 
  (Bind
    (Variable "$X")
    (Inheritance (Variable "$X") (Concept "Layer"))
    (Variable "$X")))
```

### Parsing TOML Hypergraph

```python
import tomli  # or tomllib in Python 3.11+

with open('tiny_transformer_hypergraph.toml', 'rb') as f:
    data = tomli.load(f)

# Access metadata
print(data['metadata']['model_name'])

# Iterate vertices
for vertex_id, vertex_data in data['vertices'].items():
    print(f"{vertex_id}: {vertex_data['type']}")

# Analyze hyperedges
for edge_id, edge_data in data['hyperedges'].items():
    print(f"{edge_id}: {edge_data['sources']} -> {edge_data['targets']}")

# Access weights
if 'weights' in data:
    for param_id, weights in data['weights'].items():
        print(f"{param_id} shape: {len(weights)}x{len(weights[0])}")
```

---

## Choosing the Right Format

### For Production Inference
→ Use **GGUF** with llama.cpp

### For Training/Research
→ Use **PyTorch** (.pth) format

### For Debugging
→ Use **JSON Full** or **TOML Hypergraph**

### For Documentation
→ Use **Symbolic Markdown** or **AIML**

### For Graph Analysis
→ Use **Hypergraph JSON** or **TOML Hypergraph**

### For Mathematical Analysis
→ Use **Symbolic LaTeX** or **Symbolic JSON**

### For Chatbots/Education
→ Use **AIML**

### For Cognitive AI/Symbolic Reasoning
→ Use **OpenCog AtomSpace**

### For Cross-Platform Deployment
→ Use **ONNX**

---

## Summary

The TinyTransformer model demonstrates how a single neural network can be represented in multiple formats, each optimized for different purposes:

- **3 new formats** added: AIML, OpenCog AtomSpace, TOML Hypergraph
- **15+ total formats** covering all major use cases
- **Consistent metadata** across all representations
- **Complete test coverage** (91 tests)
- **Production-ready** implementations

This multi-format approach enables:
- Seamless integration with diverse tools and frameworks
- Educational demonstrations from multiple perspectives
- Neuro-symbolic AI combining neural and symbolic reasoning
- Interactive chatbot interfaces
- Formal mathematical analysis
- Production deployment across platforms

For more details, see:
- [TinyTF README](../README.md)
- [Representation Analysis](REPRESENTATION_ANALYSIS.md)
- [Main README](../../README.md)
