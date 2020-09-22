<template>
    <div class="card">
        <div class="card-content">
            <h4><strong>{{result.name}}</strong></h4>
            <p class="outcome">{{result.outcome}}</p>
            <div v-if="result.start_time != null">
                <p>Start Time: {{dateFormat(result.start_time)}}</p>
                <p>End Time: {{dateFormat(result.end_time)}}</p>
            </div>

            <p>{{result.details}}</p>
            <slot name="options">
            </slot>
        </div>
    </div>
</template>

<script>
    import toastMixin from "../../mixins/toastMixin";
    import store from "../../store";
    import ViewActivity from "../Activities/ViewActivity";

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
        computed: {},
        props: {
            result: {
                type: Object,
                required: true
            },
        },
        methods: {
            dateFormat(date) {
                return ViewActivity.methods.dateFormat(date);
            }
        }

    }
</script>

<style scoped>
    .card-content {
        padding: 1rem;
    }

    .outcome {
        color: #4099FF;
        text-transform: uppercase;
    }
</style>