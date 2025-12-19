# Child Agent Generation Quick Reference Guide

This guide provides a quick reference for the nested-agency-coordinator to generate new child agents dynamically.

## Quick Start

1. **Read the template**: `.github/agents/child-agent-template.md`
2. **Copy and fill placeholders** with specialization-specific content
3. **Create new file**: `.github/agents/child-agent-{specialization}.md`
4. **Invoke the agent**: Reference by name `nested-agency-child-{specialization}`

## Template Placeholders

### Required Fields

| Placeholder | Description | Example |
|------------|-------------|---------|
| `{SPECIALIZATION_ID}` | Unique lowercase ID | `kubernetes-ops` |
| `{SPECIALIZATION_NAME}` | Display name | `Kubernetes Operations Specialist` |
| `{SPECIALIZATION_DESCRIPTION}` | One-line description | `Manages Kubernetes deployments and configurations` |
| `{TASK_DOMAIN}` | Domain area | `Kubernetes cluster management` |
| `{SOURCE_CONTEXT}` | Creation source | `Generated from Kubernetes documentation` |

### Content Fields

| Placeholder | Content Type | Guidelines |
|------------|--------------|-----------|
| `{SPECIALIZATION_BULLETS}` | Bulleted list | 3-7 key areas of expertise |
| `{PURPOSE_DESCRIPTION}` | 2-3 paragraphs | Explain why this agent exists |
| `{CORE_FUNCTIONS}` | Detailed list | Main capabilities and functions |
| `{EXAMPLE_TASKS}` | Bulleted examples | Concrete, actionable task examples |
| `{WORKFLOW_PATTERNS}` | Process description | Typical workflows and patterns |
| `{INTEGRATION_NOTES}` | Integration info | How this agent works with others |
| `{CONSTRAINTS}` | Limitations | Boundaries and limitations |

## File Naming Convention

```
Pattern: child-agent-{specialization}.md
Examples:
  - child-agent-kubernetes-ops.md
  - child-agent-database-migration.md
  - child-agent-api-integration.md
  - child-agent-ml-inference.md
```

## Agent Naming Convention

```yaml
# In YAML frontmatter
name: nested-agency-child-{specialization}

Examples:
  - nested-agency-child-kubernetes-ops
  - nested-agency-child-database-migration
  - nested-agency-child-api-integration
  - nested-agency-child-ml-inference
```

## Tools Selection Guide

### Standard Configuration (Recommended)
```yaml
tools: ['bash', 'create', 'edit', 'read', 'search']
```
Use for most agents that need full capabilities.

### Specialized Configurations

#### Read-Only Analysis Agent
```yaml
tools: ['read', 'search']
```
For agents that only analyze without modifying.

#### Documentation-Focused Agent
```yaml
tools: ['create', 'edit', 'read', 'search']
```
May omit `bash` if no script execution needed.

#### Execution-Heavy Agent
```yaml
tools: ['bash', 'create', 'edit', 'read', 'search']
```
Full toolset for agents that execute commands frequently.

## Generation Checklist

Before creating a new child agent:

- [ ] Confirm no existing agent covers this specialization
- [ ] Identify the source (API docs, codebase, user requirements, etc.)
- [ ] Choose unique, descriptive agent ID
- [ ] Gather specialization-specific information
- [ ] Determine appropriate tool configuration
- [ ] Plan integration with existing agents

During generation:

- [ ] Read the template file
- [ ] Replace ALL placeholder fields
- [ ] Ensure YAML frontmatter is valid
- [ ] Use consistent naming conventions
- [ ] Provide concrete example tasks
- [ ] Document integration patterns
- [ ] Specify constraints and boundaries

After generation:

- [ ] Verify agent name is unique
- [ ] Test with a simple delegation
- [ ] Document in coordinator's notes
- [ ] Update workflow documentation if needed

## Common Source Types

### 1. From API Documentation
```
Source: OpenAPI/Swagger spec for payment API
Specialization: API integration and payment processing
Task Domain: Payment gateway integration
```

### 2. From Codebase Analysis
```
Source: Analysis of authentication module
Specialization: Authentication and authorization
Task Domain: Auth system management
```

### 3. From User Requirements
```
Source: User request for "ML model deployment automation"
Specialization: ML model deployment and monitoring
Task Domain: Machine learning operations
```

### 4. From Tool/Library Documentation
```
Source: TensorFlow documentation
Specialization: TensorFlow model training and optimization
Task Domain: Deep learning with TensorFlow
```

### 5. From Domain Knowledge
```
Source: Financial domain expertise
Specialization: Financial compliance and reporting
Task Domain: Financial data processing
```

## Example: Complete Generation Process

### Scenario
User requests: "I need help integrating with the Stripe payment API"

### Step 1: Analyze Need
- **Task**: Payment API integration
- **Check existing agents**: No payment specialist exists
- **Decision**: Generate new agent

### Step 2: Gather Information
```yaml
Specialization ID: stripe-payment-integration
Display Name: Stripe Payment Integration Specialist
Task Domain: Payment gateway integration and transaction processing
Source: Stripe API documentation and user requirements
```

### Step 3: Fill Template
```markdown
---
name: nested-agency-child-stripe-payment-integration
description: Stripe payment gateway integration specialist - Handles payment processing, webhook management, and transaction workflows using Stripe API.
tools: ['bash', 'create', 'edit', 'read', 'search']
---

# Child Agent: Stripe Payment Integration Specialist

I am specialized in:
- Stripe API integration and authentication
- Payment intent creation and management
- Webhook event processing
- Subscription and billing management
- Error handling and retry logic
- PCI compliance considerations

[... fill remaining sections ...]
```

### Step 4: Create File
```bash
Path: .github/agents/child-agent-stripe-payment-integration.md
Validate: YAML frontmatter is correct
Confirm: Agent name is unique
```

### Step 5: Invoke Agent
```
Coordinator delegates: "I'll ask the nested-agency-child-stripe-payment-integration 
agent to handle the Stripe API integration."
```

### Step 6: Document
Add to coordinator's internal notes:
```
Created: nested-agency-child-stripe-payment-integration
Date: 2025-12-19
Purpose: Handle Stripe payment API integration tasks
Source: Stripe API docs + user requirements
```

## Best Practices

### 1. Naming
- Use descriptive, specific names
- Keep IDs lowercase with hyphens
- Avoid abbreviations unless widely recognized
- Make the purpose clear from the name

### 2. Specialization Scope
- Focus on a single domain or capability
- Don't make agents too broad or too narrow
- Consider typical task complexity
- Plan for agent composition if needed

### 3. Documentation
- Provide concrete, runnable examples
- Explain integration with other agents
- Document constraints and limitations
- Include workflow patterns

### 4. Tool Selection
- Start with standard tools unless specific needs
- Remove tools that won't be used
- Document why tools are included/excluded
- Consider security implications

### 5. Integration
- Explain how agent fits in nested agency
- Document data exchange patterns
- Specify coordination requirements
- Plan for multi-agent workflows

## Troubleshooting

### Issue: Agent name collision
**Solution**: Check all existing agents, use more specific ID

### Issue: Template placeholders not replaced
**Solution**: Review all `{CURLY_BRACES}` and ensure all are filled

### Issue: YAML frontmatter invalid
**Solution**: Validate YAML syntax, check quotes and brackets

### Issue: Agent not found when invoked
**Solution**: Verify filename, agent name in frontmatter, and file location

### Issue: Agent lacks needed capabilities
**Solution**: Review tools list, add missing tools, or create specialized sibling

## Additional Resources

- **Template File**: `.github/agents/child-agent-template.md`
- **Parent Agent**: `.github/agents/nested-agency.md`
- **Example Agents**: 
  - `child-agent-1.md` (Data Analysis)
  - `child-agent-2.md` (Documentation)
- **Usage Guide**: `.github/agents/USAGE (1).md`

## Version History

- **v1.0** (2025-12-19): Initial guide for dynamic agent generation
- Template version: 1.0
- Compatible with: GitHub Copilot Custom Agents

---

**Note**: This guide is intended for use by the nested-agency-coordinator parent agent. Human users should interact with the coordinator, which will handle agent generation automatically when needed.
