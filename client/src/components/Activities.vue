<template>
    <div class="container containerColor">
        <!-- Header -->
        <section class="hero level">
            <div class=" hero-body level-item">
                <div class="container containerColor">
                    <h1 class="title is-1">
                        Activities
                    </h1>
                </div>

                <b-button
                           type="is-info">
                    Add Activity
                </b-button>
            </div>
        </section>

        <hr class ="hrLine">

        <div class="container containerColor has-same-height is-gapless">
            <div class="column">
                <!-- Activities -->
                <div class="card">
                    <div class="card-content">
                        <h3 class="title is-4">Name</h3>

                        <div class="content">
                            <table class="table-profile">
                                <tr>
                                    <th colspan="1"></th>
                                    <th colspan="2"></th>
                                </tr>
                                <tr>
                                    <td>Description:</td>
                                    <td>none</td>
                                </tr>
                                <tr>
                                    <td>Continous/Duration:</td>
                                    <td>cont</td>
                                </tr>
                                <tr>
                                    <td>Start Time:</td>
                                    <td>none</td>
                                </tr>
                                <tr>
                                    <td>End Time:</td>
                                    <td>none</td>
                                </tr>
                                <tr>
                                    <td>Location:</td>
                                    <td>Bob's House</td>
                                </tr>
                            </table>
                        </div>
                        <br>
                    </div>
                </div>
            </div>
        </div>
    </div>
</template>

<script>

    export default {
        name: "Activities"
    }

</script>

<style scoped>
    .bannerColor {
        background-color: #64C6E3
    }

    .containerColor {
        background-color: #F7F8F9
    }

    .center {
        text-align: center;
    }

    .hrLine {
        border:2px solid #EDEEEE;
    }

</style>