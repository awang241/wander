<template>
    <div class="container containerColor">

        <img class="mainpage-banner" src="../../images/WANDER-day-navbar.png"/>
        <div class="container has-text-centered">
            <h2 class="title">Our Philosophy</h2>
            <p>
                We believe being active and wandering out into new places all over the world is an important part of what
                makes us human. At the same time staying connected with friends, family and meeting new people is integral to the human experience.
                Stay active, connect and wander with WANDER.
            </p>
        </div>

        <div class="columns is-centered" style="padding: 2rem">
            <div class="column">
                <div class="card">
                    <div class="card-image">
                        <figure class="image is-2by1">
                            <img class="crop-banner" src="../../images/cycling.jpg"/>
                        </figure>
                    </div>
                    <div class="card-content">
                        <div class="media">
                            <div class="media-content">
                                <p class="title is-4">Stay Active</p>
                            </div>
                        </div>

                        <div class="content">
                            Use our app to stay active. With users everywhere around the world, you will
                            always be able to find new activities to partake in.
                        </div>
                    </div>
                </div>
            </div>

                    <div class="column">
                        <div class="card">
                            <div class="card-image">
                                <figure class="image is-2by1">
                                    <img class="crop-banner" src="../../images/basketball.jpg"/>
                                </figure>
                            </div>
                            <div class="card-content">
                                <div class="media">
                                    <div class="media-content">
                                        <p class="title is-4">Connect</p>
                                    </div>
                                </div>

                                <div class="content">
                                    Connect with friends and family, or even meet new people! Use WANDER to stay
                                    connected. We make it easy!
                                </div>
                            </div>
                        </div>
                    </div>
                    <div class="column">
                        <div class="card">
                            <div class="card-image">
                                <figure class="image is-2by1">
                                    <img class="crop-banner" src="../../images/hiking.jpg"/>
                                </figure>
                            </div>
                            <div class="card-content">
                                <div class="media">
                                    <div class="media-content">
                                        <p class="title is-4">Wander</p>
                                    </div>
                                </div>

                                <div class="content">
                                    Unsure where to take your next travel? With WANDER you will have limitless ideas
                                    and destinations you can wander to!
                                </div>
                            </div>
                        </div>
                    </div>

                </div>
        <section class="section">
            <div class="columns">
                <div class="column is-8 is-offset-2">
                    <div class="content is-medium">
                        <h1 class="title">Getting Started</h1>
                        <p>Its easy! Register today with a free account and stay updated with the latest events in your area
                        and around the globe!</p>
                        <b-button
                                  tag="router-link"
                                  to="/Registration"
                                  type="is-primary">
                            Register
                        </b-button>
                    </div>
                </div>
            </div>
        </section>

        <hr>
        <div class="container has-text-centered">
            <h2 class="title">Testimonials</h2>

            <div class="tile is-ancestor">
                <div class="tile is-parent">
                    <article class="tile is-child box">
                        <p class="title">Barry</p>
                        <p class="subtitle">03/04/2020, Christchurch, NZ</p>
                        <p><i>I met my current wife on this website on our hiking trip!</i> </p>
                    </article>
                </div>
                <div class="tile is-parent">
                    <article class="tile is-child box">
                        <p class="title">Joe</p>
                        <p class="subtitle">02/07/2020, Tibet</p>
                        <p><i>What a fantastic app. Through this app, I was able to meet like minded people
                        and start a new hobby.</i> </p>
                    </article>
                </div>
                <div class="tile is-parent">
                    <article class="tile is-child box">
                        <p class="title">Kate</p>
                        <p class="subtitle">05/01/2020, New York, USA</p>
                        <p><i>Better than facebook!</i> </p>
                    </article>
                </div>
                <div class="tile is-parent">
                    <article class="tile is-child box">
                        <p class="title">Lisa</p>
                        <p class="subtitle">12/02/2020, Osaka, Japan</p>
                        <p><i>Great way to stay active and meet new people. Love it!
                        Used it while I was travelling around the world</i> </p>
                    </article>
                </div>
                <div class="tile is-parent">
                    <article class="tile is-child box">
                        <p class="title">Peter</p>
                        <p class="subtitle">02  3/07/2020, Sydney, Australia</p>
                        <p><i>Awesome app to plan stuff with the boys like weekend basketball.</i> </p>
                    </article>
                </div>
            </div>
        </div>
    </div>
</template>


<script>
    export default {
        name: "Mainpage"
    }
</script>

<style>
    .mainpage {
        line-height: 12px;
        font-size: 16pt;
        margin-top: 60px;
    }
    html {
        background-size:cover;
        background: url(https://www.goodfreephotos.com/albums/new-zealand/other-new-zealand/snow-capped-peaks-and-mountains-landscape-in-new-zealand.jpg) no-repeat center center fixed;
        -webkit-background-size: cover;
        -moz-background-size: cover;
        -o-background-size: cover;
        background-size: cover;
    }
    .mainpage-banner {
        /*width: 1550px;*/
        /*height: 1250px;*/
        overflow: hidden;
        margin-left: auto;
        margin-right: auto;
        display: block;

    }
    .mainpage-banner img {
        /*width: 1550px;*/
        /*height:1250px;*/
        /*margin: -75px 0 0 -100px;*/
        margin-left: auto;
        margin-right: auto;
        display: block;
    }

</style>