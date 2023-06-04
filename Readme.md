## Bank Application

## Requerimientos

Para construir esta aplicacion ud necesita:

- [JDK 11](https://jdk.java.net/11/)
- [Maven 3](https://maven.apache.org)
- [PostgreSql >=4](https://www.mongodb.com/try/download/community) o [Docker](https://www.docker.com/)

## Correr la aplicacion localmente

<ol>
<li>
<h4>Para desplegar la aplicación en local haciendo uso de Docker  es necesario contar con :</h4>
- Docker<br> 
- Docker Compose
<br>
Desde una terminal ubicada en la raiz del proyecto ejecutar:
<br><code>mvn compile jib:dockerBuild ; docker-compose -f src/main/docker/app.yml up</code>
</li>
<li>Tambien puede utilizar Spring Boot Maven plugin:
  <br><code>mvn spring-boot:run -Dspring-boot.run.profiles=dev</code>
</li>
<li>
Para ejecutar la aplicacion Spring Boot en tu maquina local  es necesario ejecutar el  <code>main</code> metodo en la clase 
<code>co.com.mercado.libre.prueba.SpringApplication</code> desde su IDE.
</li>
</ol>