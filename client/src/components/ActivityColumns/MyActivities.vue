<template>
    <div class="container containerColor">

        <div class="has-same-height is-gapless">
            <h3 class="title is-1">
                My activities
            </h3>
        </div>
    </div>
</template>

<script>
    import store from "../../store";
    import toastMixin from "../../mixins/toastMixin";

    export default {
        name: "MyActivities",
        mixins: [toastMixin],
        data() {
            return {
                activities: null,
                store: store
            }
        }
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
        border: 2px solid #EDEEEE;
    }

    #editButton {
        margin-left: 1rem;
    }

    #activities-key-info{
        display: flex;
        justify-content: space-between;
        padding: 0rem 1rem;
    }

</style>