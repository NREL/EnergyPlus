FROM ubuntu:18.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
  && apt-get install -y python3-pip python-dev\
  && apt-get install -y software-properties-common \
  && add-apt-repository ppa:deadsnakes/ppa \
  && apt-get install -y python3.7 \
  && apt-get update

RUN apt-get install -y python3-pip python3.7-dev \
  && pip3 install --upgrade pip \
  && apt-get install -y curl \
  && apt-get update

# venv build
# without the use of VIRTUAL_ENV the venv will not activate or be seen
ENV VIRTUAL_ENV=/opt/venv
RUN python3.7 -m venv $VIRTUAL_ENV --without-pip
ENV PATH="$VIRTUAL_ENV/bin:$PATH"
RUN curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
RUN python get-pip.py
RUN rm get-pip.py
RUN python --version
RUN pip --version

ADD ./ /home/project/pyExpandObjects
WORKDIR /home/project/pyExpandObjects/

RUN python -m pip install -r requirements.txt

RUN pyinstaller linux_onefile_main.spec
#RUN pyinstaller main.spec
