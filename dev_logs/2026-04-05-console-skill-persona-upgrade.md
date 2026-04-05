# 2026-04-05 Console Skill / Persona Upgrade

## 背景

这轮开发最初是从两个实际问题开始的：

1. `console_chat()` 默认没有稳定接入内置 skills。
2. 即使 skill 已经被加载，像 `Y叔在吗？` 这种输入也不能稳定触发对应 persona / style。

在排查后，问题被拆成三层：

- 启动层：`console_chat()` 默认 agent 没有把 skills 自动接进来。
- 路由层：skill 触发主要依赖模型自行判断，没有显式 persona / alias 命中机制。
- 会话层：console 没有独立的人格状态，skill 只能做单轮 prompt 覆盖，不能形成持续 persona。

## 目标

本轮目标不是只修一个 bug，而是把 console 的能力补成一条完整链路：

- 启动时默认可用本地 / 内置 skills
- 模型选择只在首次或失效时触发，平时直接复用
- skill frontmatter 升级，不再只有 `name + description`
- 显式提到 persona / alias 时，能够稳定命中 skill
- console 本身拥有会话级 persona 机制，且能被 skill persona 自动覆盖

## 本轮改动

### 1. Console 默认技能接入

- `create_console_agent()` 现在默认使用 `skills = "auto"`。
- 这意味着 `getwd()/inst/skills`、包内 `system.file("skills", package = "aisdk")` 等标准位置会自动扫描。

影响：

- `console_chat()` 启动后默认具备 `load_skill`、`read_skill_resource`、`execute_skill_script` 等 skill tools。
- skill 列表会进入 agent system prompt。

### 2. Console 默认模型记忆

- 新增 `.Rprofile` 级默认模型记忆层。
- 启动 `console_chat()` 时先尝试读取保存的默认模型。
- 如果默认模型无效、缺少对应 provider 配置、或创建 session 失败，自动回退到原来的交互式模型设置。
- 模型设置成功后会回写 `.Rprofile`，后续不再每次询问。

关键点：

- `.Renviron` 继续保存 provider 的 key / base_url / model
- `.Rprofile` 额外保存 `aisdk.console_default_model`

### 3. Skill 元数据升级

`Skill` / `SkillRegistry` 现在支持更丰富的 frontmatter：

- `aliases`
- `when_to_use`
- `paths`

对应能力：

- alias 解析
- fuzzy skill name recovery
- 文本匹配
- 路径匹配
- 按 score 返回 relevant skills

这一层借鉴了 Claude Code skill 系统里“frontmatter 只放轻量路由信息，正文只在真正命中时加载”的思路，但仍保持 `aisdk` 当前架构兼容。

### 4. Console 路由统一到 SkillRegistry

之前 console 的 skill 匹配逻辑是临时写在 `console.R` 里的。

本轮把它收敛为统一的 registry API：

- `resolve_skill_name()`
- `find_closest_skill_name()`
- `find_relevant_skills(query, file_paths, cwd, limit)`

console 现在只负责：

- 从用户输入里提取 query 和可能的本地路径
- 调 registry 找候选 skill
- 对命中的 skill 生成 turn-scoped prompt

### 5. Persona 机制

新增 `console_persona.R`，引入 session-scoped persona state。

当前 persona 结构：

- `default`：内置默认人格
- `active`：当前生效人格

人格来源：

- `default`
- `skill`
- `manual`

行为规则：

- 默认总有一个基础 persona
- 命中带 `persona.md` 的 skill 时，自动切换到 skill persona
- 用户手动设置的 persona 优先级更高，并可锁定
- 当前 persona 可通过 note 做“进化”

### 6. Persona 命令

新增 `/persona` 命令族：

- `/persona`
- `/persona set <instructions>`
- `/persona skill <skill_name>`
- `/persona evolve <note>`
- `/persona default`

同时状态栏新增 `Persona:` 字段，方便观察当前 persona 是否已经切换。

### 7. Y叔 skill 修正

针对 `yshu`：

- 增强了 `description`
- 增加了 alias：`Y叔`、`yshu`、`余叔`、`Y叔风格` 等
- 读取 `persona.md` 作为 persona prompt
- 如存在 `meta.json`，优先用其中更自然的 persona label

注意：

- `inst/skills/yshu/SKILL.md` 当前在工作树里仍是未跟踪文件，需要后续决定是否纳入版本控制

## 参考与学习

本轮专门对照阅读了：

- `claude-code-source-code-main/src/skills/loadSkillsDir.ts`
- `claude-code-source-code-main/src/tools/SkillTool/prompt.ts`
- `claude-code-source-code-main/src/commands.ts`
- `claude-code-source-code-main/src/utils/attachments.ts`
- `claude-code-source-code-main/src/query.ts`

得到的主要启发：

- 不能只靠“把 skills 列到 system prompt”来触发
- frontmatter 应该承担轻量路由职责
- `whenToUse` 比单纯 description 更适合做触发语义
- skill 正文应该按需加载，而不是常驻
- persona / skill / tool invocation 最好拆层处理，而不是混在同一段系统提示里

## 验证

本轮已通过的关键测试包括：

```r
Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-setup.R', reporter='summary')"
Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-skill.R', reporter='summary')"
Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-skill-registry.R', reporter='summary')"
Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-agent.R', reporter='summary')"
Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-console-ui.R', reporter='summary')"
Rscript -e "pkgload::load_all('.'); testthat::test_file('tests/testthat/test-session-agent.R', reporter='summary')"
```

覆盖点：

- `.Rprofile` 默认模型记忆
- skill metadata 解析
- alias / fuzzy / relevant skill matching
- console skill routing
- skill persona 自动切换
- `/persona` 命令行为
- persona 在状态栏中的显示

## 当前局限

虽然这轮已经把 console 做成了“默认技能 + skill 路由 + persona 状态”的闭环，但还有几个明显没做完的地方：

1. channel/runtime 还没有复用 persona 机制。
2. `paths` 目前主要依赖用户输入里出现路径，还没有和真实文件操作记录做强绑定。
3. 还没有独立的 skill discovery prefetch 层。
4. persona 的“学习/进化”目前只是追加 notes，没有形成结构化持久记忆。
5. 尚未跑 `devtools::document()` 和整包 `R CMD check`。

## 下一步建议

优先级建议：

1. 把 persona 机制扩展到 channel runtime。
2. 把 `paths` skill activation 接到真实文件读写/edit 事件。
3. 为 persona 增加结构化存储与恢复。
4. 评估是否需要独立的 skill discovery attachment，而不是继续增加 system prompt 压力。

