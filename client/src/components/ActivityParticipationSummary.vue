<template>
    <div class="card">
        <div class="columns">
            <div class="column">
                <h4><strong>{{name}}</strong></h4>
                <p>{{profile.gender}}</p>
                <p>{{profile.email}}</p>
            </div>
            <div class="column">
                <p class="color-primary">{{result.outcome}}</p>
                <div v-if="result.startTime != null">
                    <p>Start Time: {{result.startTime}}</p>
                    <p>End Time: {{result.endTime}}</p>
                </div>
            </div>
            <p>{{result.details}}</p>
            <slot name="options">
            </slot>
        </div>
    </div>
</template>

<script>
    import toastMixin from "../mixins/toastMixin";
    import store from "../store";

    export default {
        name: "ActivityParticipationSummary",
        mixins: [toastMixin],
        data() {
            return {
                resultData: {},
                store: store
            }
        },
        mounted() {
            this.resultData = this.$props.result;
        },
        computed: {
            // name() {
            //     return `${this.result.firstname}  ${this.result.lastname}`
            // },
            // profileIsAdmin(){
            //     return this.result.authLevel < 2
            // }
        },
        props: {
            result: {
                type: Object,
                required: true
            }
        }
    }
</script>

<style scoped>
    .columns {
        padding: 1rem;
    }
    li{
        list-style-type: none;
    }

    .color-primary {
        color: #4099FF
    }
</style>