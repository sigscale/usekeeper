FROM marketplace.gcr.io/google/ubuntu1804

RUN apt-get -y update && apt-get -y install \
	erlang-base \
	erlang-mnesia \
	erlang-ssl \
	erlang-inets \
	&& rm -rf /var/lib/apt/lists/*

COPY 1.1.0.tar.gz /usr/lib/erlang/releases/1.1.0.tar.gz
RUN cd /usr/lib/erlang && \
	tar zxf releases/1.1.0.tar.gz && \
	erl -noinput \
		-eval "application:start(sasl)" \
		-eval "release_handler:set_unpacked(\"releases/1.1.0.rel\",[])" \
		-s init stop && \
	sed -i 's/unpacked/current/' releases/RELEASES && \
	erl -noinput \
		-eval "application:start(sasl)" \
		-eval "release_handler:make_permanent(\"1.1.0\")" \
		-s init stop

RUN adduser --gecos "Erlang/OTP Embedded System" \
	--disabled-password otp
USER otp
WORKDIR /home/otp
ENV ROOTDIR /usr/lib/erlang
ENV RELDIR /usr/lib/erlang/releases
ENV OTPHOME $ROOTDIR

RUN mkdir /home/otp/db && \
	mkdir /home/otp/bin &&\
	mkdir /home/otp/log && \
	mkdir /home/otp/log/sasl && \
	mkdir /home/otp/log/usage && \
	mkdir /home/otp/log/http && \
	mkdir /home/otp/log/ipdr && \
	mkdir /home/otp/log/export &&\
	mkdir /home/otp/ssl && \
	openssl req -newkey rsa:2048 -nodes -x509 -days 1024 -subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=$(hostname)\/emailAddress=support@$(hostname) -keyout ssl/cakey.pem -out ssl/ca.pem && \
	openssl req -newkey rsa:2048 -nodes -subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=$(hostname)\/emailAddress=support@$(hostname) -keyout ssl/key.pem -out ssl/cert.csr && \
	echo "extendedKeyUsage = serverAuth" > ssl/extensions && \
	echo "subjectAltName = DNS:$(hostname)" >> ssl/extensions && \
	openssl x509 -req -days 1024 -CA ssl/ca.pem -CAkey ssl/cakey.pem -CAcreateserial -extfile ssl/extensions -in ssl/cert.csr -out ssl/cert.pem && \
	openssl x509 -outform DER -in ssl/ca.pem -out ssl/ca.der && \
	chmod 400 ssl/key.pem ssl/cakey.pem

COPY start initialize /home/otp/bin/

EXPOSE 8080/tcp

ENTRYPOINT ["bin/start"]

MAINTAINER SigScale <info@sigscale.org>

