# ==================================================================================================
FROM debian:12 AS opinionated-dev-setup
# ==================================================================================================

# Some install scripts assume bash, this makes following RUN commands use `bash` instead of `sh`.
# The `-c` flag says: "accept a command instead of starting interactive mode".
SHELL ["/bin/bash", "-c"]

# --------------------------------------------------------------------------------------------------
# Basic utilities

# Set sane defaults for the year 2026
ENV TZ=UTC
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV TERM=xterm-256color
ENV COLORTERM=truecolor
ENV EDITOR=hx

# Set a non-interactive mode for apt and install necessary tools
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential \
      ca-certificates \
      curl \
      dirmngr \
      git \
      gnupg \
      locales \
      lsb-release \
      ncdu \
      openssh-client \
      software-properties-common \
      sudo \
      wget \
      xz-utils \
    && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
    && locale-gen \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# --------------------------------------------------------------------------------------------------
# Install helix editor

ARG HELIX_VERSION=25.07.1
RUN HELIX_FILE_VERSION=$(echo ${HELIX_VERSION} | sed 's/\.0/./g') \
    && curl -LO "https://github.com/helix-editor/helix/releases/download/${HELIX_VERSION}/helix_${HELIX_FILE_VERSION}-1_amd64.deb" \
    && dpkg -i helix_${HELIX_FILE_VERSION}-1_amd64.deb \
    && rm helix_${HELIX_FILE_VERSION}-1_amd64.deb

# --------------------------------------------------------------------------------------------------
# Github cli
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
    && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
    && apt-get update \
    && apt-get install -y gh \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# --------------------------------------------------------------------------------------------------
# Dev user account

# Create a user 'vscode' with a UID that matches the host (default 1000)
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create the user
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m -s /bin/bash $USERNAME \
    # Allow no-password sudo
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME
    
# Switch to this user
USER $USERNAME

# =============================================================================
FROM debian:12 AS ai-layer
# =============================================================================

RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ARG NVM_VERSION=0.40.3
ARG NODEJS_VERSION=24

ENV NVM_DIR=/opt/nvm
ENV NODE_PATH=$NVM_DIR/versions/node/v${NODEJS_VERSION}/lib/node_modules
ENV PATH=$NVM_DIR/versions/node/v${NODEJS_VERSION}/bin:$PATH

RUN mkdir -p $NVM_DIR \
    && curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v${NVM_VERSION}/install.sh | bash \
    && . "$NVM_DIR/nvm.sh" \
    && nvm install $NODEJS_VERSION \
    && nvm alias default $NODEJS_VERSION \
    && nvm use $NODEJS_VERSION \
    && npm install -g @anthropic-ai/claude-code @google/gemini-cli \
    && nvm cache clear \
    && npm cache clean --force

# Store the actual node version for use in final stage
RUN . "$NVM_DIR/nvm.sh" \
    && NODE_VERSION_FULL=$(node --version) \
    && echo "$NODE_VERSION_FULL" > /opt/nvm/.node_version

# =============================================================================
FROM debian:12 AS r-layer
# =============================================================================

RUN apt-get update && apt-get install -y --no-install-recommends \
      r-base \
      # Build tools for compiling R packages from source
      build-essential \
      # Undeclared dependencies of various common r packages
      gfortran \
      libblas-dev \
      libcurl4-openssl-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libfribidi-dev \
      libharfbuzz-dev \
      libjpeg-dev \
      liblapack-dev \
      libpng-dev \
      libssh-dev \
      libssl-dev \
      libtiff5-dev \
      libxml2-dev \
      pandoc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# R packages: VSCode integration
# See https://github.com/REditorSupport/vscode-R/issues/1588 for httpgd repo
# See https://github.com/REditorSupport/vscode-R/issues/1660 for languageserver fix (.lintr ignored)
RUN <<EOF
    R -e "install.packages('remotes', Ncpus=parallel::detectCores())"
    R -e "remotes::install_github('REditorSupport/languageserver@pull/706/head')"
    # plotting support
    R -e "install.packages(
        'httpgd',
        repos = c('https://community.r-multiverse.org', 'https://cloud.r-project.org'),
        Ncpus=parallel::detectCores()
    )"
    # debugger support
    R -e "install.packages(
        'vscDebugger',
        repos = 'https://manuelhentschel.r-universe.dev'
    )"
EOF

# R packages: project dependencies
RUN R -e "install.packages(c('devtools', 'usethis'), Ncpus=parallel::detectCores())"

# ==================================================================================================
FROM opinionated-dev-setup AS final
# ==================================================================================================

# Switch to root for installing packages
USER root

# --------------------------------------------------------------------------------------------------
# R runtime dependencies

RUN apt-get update && apt-get install -y --no-install-recommends \
      r-base \
      libcurl4-openssl-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libfribidi-dev \
      libharfbuzz-dev \
      libjpeg-dev \
      libpng-dev \
      libssh-dev \
      libssl-dev \
      libtiff5-dev \
      libxml2-dev \
      gfortran \
      libblas-dev \
      liblapack-dev \
      pandoc \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# --------------------------------------------------------------------------------------------------
# Copy software from builder stages

# AI tools (node, claude, gemini)
ENV NVM_DIR=/opt/nvm
COPY --from=ai-layer /opt/nvm /opt/nvm

# Set up PATH using the actual node version installed
RUN NODE_VERSION=$(cat /opt/nvm/.node_version) \
    && NODE_BIN_DIR="/opt/nvm/versions/node/${NODE_VERSION}/bin" \
    && echo "export PATH=${NODE_BIN_DIR}:\$PATH" >> /etc/profile.d/nodejs.sh \
    && chmod +x /etc/profile.d/nodejs.sh \
    # Create symlinks for common commands to work without sourcing profile
    && ln -s "${NODE_BIN_DIR}/node" /usr/local/bin/node \
    && ln -s "${NODE_BIN_DIR}/npm" /usr/local/bin/npm \
    && ln -s "${NODE_BIN_DIR}/npx" /usr/local/bin/npx \
    && ln -s "${NODE_BIN_DIR}/claude" /usr/local/bin/claude \
    && ln -s "${NODE_BIN_DIR}/gemini" /usr/local/bin/gemini

# R packages (site-library)
COPY --from=r-layer /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# --------------------------------------------------------------------------------------------------
# Switch to dev user

USER vscode

ENTRYPOINT ["bash"]